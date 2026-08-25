// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Applicative
import cats.Endo
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import coulomb.integrations.cats.all.given
import fs2.Stream
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig as OcsStepConfig
import lucuma.core.refined.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.function.Index.mapIndex
import mouse.all.*
import observe.cats.given
import observe.model.*
import observe.model.UserPrompt.Discrepancy
import observe.model.UserPrompt.ObsConditionsCheckOverride
import observe.model.UserPrompt.SeqCheck
import observe.model.UserPrompt.TargetCheckOverride
import observe.model.config.*
import observe.model.enums.BatchExecState
import observe.model.enums.ObserveLogLevel
import observe.model.enums.PendingObserveCmd
import observe.model.enums.PendingObserveCmd.*
import observe.model.enums.Resource
import observe.model.enums.RunOverride
import observe.model.events.*
import observe.server.engine.EngineStep
import observe.server.engine.EventResult.*
import observe.server.engine.Handle.given
import observe.server.engine.Result.Partial
import observe.server.engine.{EngineStep as _, *}
import observe.server.events.*
import observe.server.odb.{OdbObservationData, OdbProxy}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

import java.util.concurrent.TimeUnit
import scala.annotation.unused
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*
import SeqEvent.*
import ClientEvent.*

private class ObserveEngineImpl[F[_]: {Async, Logger as L}](
  executeEngine:         Engine[F],
  override val systems:  Systems[F],
  @unused settings:      ObserveEngineConfiguration,
  translator:            SeqTranslate[F],
  @unused conditionsRef: Ref[F, CurrentConditions]
) extends ObserveEngine[F] {

  /**
   * Check if the resources to run a sequence are available
   * @return
   *   true if resources are available
   */
  private def checkResources(obsId: Observation.Id)(st: EngineState[F]): Boolean = {
    // Resources used by running sequences
    val used: Set[Subsystem] = ObserveEngine.resourcesInUse(st, excludeObs = obsId.some)

    // Resources that will be used by sequences in running queues
    val reservedByQueues: Set[Subsystem] = ObserveEngine.resourcesReserved(st)

    st.sequences
      .get(obsId)
      .exists: seqData =>
        seqData.resources.intersect(used).isEmpty && (
          st.queues.values.filter(_.status.running).exists(_.queue.contains(obsId)) ||
            seqData.resources.intersect(reservedByQueues).isEmpty
        )
  }

  /**
   * Check if the target on the TCS matches the Observe target
   * @return
   *   an F that returns an optional TargetMatchResult if the targets don't match
   */
  private def sequenceTcsTargetMatch(
    seqData: SequenceData[F]
  ): F[Option[TargetCheckOverride]] =
    seqData.targetEnvironment.firstScienceTarget
      .map(_.targetName.toString)
      .map: seqTarget =>
        systems.tcsKeywordReader.sourceATarget.objectName.map: tcsTarget =>
          (seqTarget =!= tcsTarget).option:
            TargetCheckOverride(UserPrompt.Discrepancy(tcsTarget, seqTarget))
      .getOrElse(none.pure[F])

  private def stepRequiresChecks(stepConfig: OcsStepConfig): Boolean =
    stepConfig match
      case OcsStepConfig.Gcal(_, _, _, _) => true
      case OcsStepConfig.Science          => true
      case OcsStepConfig.SmartGcal(_)     => true
      case _                              => false

  private def checkCloudCover(
    actual:    Option[CloudExtinction],
    requested: CloudExtinction.Preset
  ): Boolean =
    actual.forall(_ <= requested.toCloudExtinction)

  private def checkImageQuality(
    actual:    Option[ImageQuality],
    requested: ImageQuality.Preset
  ): Boolean =
    actual.forall(_ <= requested.toImageQuality)

  private def checkSkyBackground(
    actual:    Option[SkyBackground],
    requested: SkyBackground
  ): Boolean =
    actual.forall(_ <= requested)

  private def checkWaterVapor(actual: Option[WaterVapor], requested: WaterVapor): Boolean =
    actual.forall(_ <= requested)

  private def observingConditionsMatch(
    actualObsConditions:   CurrentConditions,
    requiredObsConditions: ConstraintSet
  ): Option[ObsConditionsCheckOverride] = {

    val UnknownStr: String = "Unknown"
    val reqCE              = requiredObsConditions.cloudExtinction
    val reqIQ              = requiredObsConditions.imageQuality
    val reqSB              = requiredObsConditions.skyBackground
    val reqWV              = requiredObsConditions.waterVapor

    val ccCmp = (!checkCloudCover(actualObsConditions.ce, reqCE))
      .option(
        Discrepancy(
          actualObsConditions.ce.fold(UnknownStr)(_.label),
          reqCE.toCloudExtinction.label
        )
      )

    val iqCmp = (!checkImageQuality(actualObsConditions.iq, reqIQ))
      .option(
        Discrepancy(
          actualObsConditions.iq.fold(UnknownStr)(_.label),
          reqIQ.toImageQuality.label
        )
      )

    val sbCmp = (!checkSkyBackground(actualObsConditions.sb, reqSB))
      .option(Discrepancy(actualObsConditions.sb.fold(UnknownStr)(_.label), reqSB.label))

    val wvCmp = (!checkWaterVapor(actualObsConditions.wv, reqWV))
      .option(Discrepancy(actualObsConditions.wv.fold(UnknownStr)(_.label), reqWV.label))

    (ccCmp.nonEmpty || iqCmp.nonEmpty || sbCmp.nonEmpty || wvCmp.nonEmpty)
      .option(ObsConditionsCheckOverride(ccCmp, iqCmp, sbCmp, wvCmp))
  }

  private def clearObsCmd(obsId: Observation.Id): EngineHandle[F, SeqEvent] =
    EngineHandle.modifyState:
      EngineState
        .atSequence[F](obsId)
        .andThen(SequenceData.pendingObsCmd)
        .replace(None)
        .withEvent:
          SeqEvent.NullSeqEvent: SeqEvent

  private def setObsCmd(obsId: Observation.Id, cmd: PendingObserveCmd): EngineHandle[F, SeqEvent] =
    EngineHandle.modifyState:
      EngineState
        .atSequence[F](obsId)
        .andThen(SequenceData.pendingObsCmd)
        .replace(cmd.some)
        .withEvent:
          SeqEvent.NullSeqEvent: SeqEvent

  // Produce a Handle that will send a SequenceStart notification to the ODB, and produces the (sequenceId, stepId)
  // if there is a valid sequence with a valid current step.
  private def sequenceStart(
    obsId: Observation.Id
  ): EngineHandle[F, Option[(Observation.Id, Step.Id)]] =
    EngineHandle.getState.flatMap { s =>
      EngineState
        .atSequence(obsId)
        .getOption(s)
        .flatMap { seq =>
          val startVisit: EngineHandle[F, SeqEvent] =
            if (!seq.visitStartDone)
              EngineHandle
                .fromEventStream:
                  Stream.eval[F, Event[F]]:
                    systems.odb
                      .visitStart(obsId)
                      .as:
                        Event.modifyState:
                          EngineHandle
                            .modifyState_ :
                              EngineState.atSequence[F](obsId).modify(_.withCompleteVisitStart)
                            .as(SeqEvent.NullSeqEvent)
                .as(SeqEvent.NullSeqEvent)
            else
              EngineHandle.pure(SeqEvent.NullSeqEvent)

          seq.seq.loadedStep.map: curStep =>
            (startVisit *>
              Handle
                .fromEventStream[F, EngineState[F], Event[F]](
                  Stream.eval[F, Event[F]](
                    systems.odb
                      .sequenceStart(obsId)
                      .as(Event.nullEvent)
                  )
                )).as((obsId, curStep.id).some)
        }
        .getOrElse(EngineHandle.pure(none[(Observation.Id, Step.Id)]))
    }

  private def startAfterCheck(
    startAction: EngineHandle[F, Unit],
    id:          Observation.Id
  ): EngineHandle[F, SeqEvent] =
    startAction.reversedStreamFlatMap: _ =>
      sequenceStart(id).map:
        _.map: (sid, stepId) =>
          SequenceStart(sid, stepId)
        .getOrElse(NullSeqEvent)

  private def startChecks(
    startAction:    EngineHandle[F, Unit],
    obsId:          Observation.Id,
    clientId:       ClientId,
    @unused stepId: Option[Step.Id],
    runOverride:    RunOverride
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap { st =>
      EngineState
        .atSequence(obsId)
        .getOption(st)
        .filter(seqData => seqData.seq.status.isIdle || seqData.seq.status.isError)
        .map: seq =>
          (for {
            _              <- EngineHandle.modifySequenceState[F](obsId): seq =>
                                SequenceState.status.replace(SequenceStatus.Running.Starting)(seq.rollback)
            startingStep   <-
              ObserveEngine.retrieveStep(
                systems.odb,
                translator,
                seq.obsId,
                seq.currentSequenceType.asLeft
              )
            checkedStepOpt  = startingStep.filter(step => stepRequiresChecks(step.config))
            (stpg, checks) <- EngineHandle.liftF:
                                checkedStepOpt
                                  .map: checkedStep =>
                                    sequenceTcsTargetMatch(seq).map: tchk =>
                                      (checkedStep.some,
                                       List(
                                         tchk,
                                         observingConditionsMatch(st.conditions, seq.constraintSet)
                                       ).flattenOption
                                      )
                                  .getOrElse((none[StepGen[F]], List.empty[SeqCheck]).pure[F])
            resultEvent    <-
              EngineHandle.getState
                .flatMap: newState =>
                  (checkResources(obsId)(newState), stpg, checks, runOverride) match
                    // Resource check fails
                    case (false, _, _, _)                             =>
                      EngineHandle
                        .modifySequenceState[F](obsId)(_.withNoLoadedStep.withIdleStatus)
                        .as[SeqEvent](Busy(obsId, clientId))
                    // Target check fails and no override
                    case (_, Some(stp), x :: xs, RunOverride.Default) =>
                      EngineHandle
                        .modifySequenceState[F](obsId)(_.withNoLoadedStep.withIdleStatus)
                        .as[SeqEvent](
                          RequestConfirmation(
                            UserPrompt.ChecksOverride(obsId, stp.id, NonEmptyList(x, xs)),
                            clientId
                          )
                        )
                    // Allowed to run
                    case _                                            =>
                      startAfterCheck(startAction, obsId)
          } yield resultEvent).handleErrorWith(e =>
            EngineHandle
              .modifySequenceState[F](obsId)(_.withNoLoadedStep.withFailedStatus(e.getMessage))
              .as[SeqEvent](
                SeqEvent.NotifyUser(
                  Notification.LoadingFailed(
                    obsId,
                    List(s"Error loading sequence for observation $obsId", e.getMessage)
                  ),
                  clientId
                )
              )
          )
        .getOrElse: // Trying to run a sequence that does not exist. This should never happen.
          EngineHandle.unit.as[SeqEvent](NullSeqEvent)
    }

  // Stars a sequence from the first non executed step. The method checks for resources conflict.
  override def start(
    obsId:       Observation.Id,
    user:        User,
    observer:    Observer,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Start requested by ${user.displayName}") *>
      executeEngine.offer:
        Event.modifyState[F](
          setObserver(obsId, observer) *>
            clearObsCmd(obsId) *>
            startChecks(executeEngine.startLoadedStep(obsId), obsId, clientId, none, runOverride)
        )

  override def proceedAfterPrompt(
    id:       Observation.Id,
    user:     User,
    observer: Observer,
    seqType:  SequenceType
  ): F[Unit] =
    setObserver(id, user, observer) *>
      executeEngine.offer:
        Event.modifyState[F]:
          clearObsCmd(id) *>
            userNextStep(id, seqType).as(SeqEvent.NullSeqEvent)

  def userNextStep(
    obsId:   Observation.Id,
    seqType: SequenceType
  ): EngineHandle[F, Unit] =
    EngineHandle.getState.flatMap: st =>
      EngineState
        .atSequence(obsId)
        .getOption(st)
        .map: _ => // Sequence exists in memory, proceed
          ObserveEngine.tryNewStep(systems.odb, translator, executeEngine, obsId, seqType)
        .getOrElse(EngineHandle.unit)

  override def requestPause(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Pause requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(Event.pause(obsId, user))

  override def requestCancelPause(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Continue requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(Event.cancelPause(obsId, user))

  override def setBreakpoints(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    steps:    Set[Step.Id],
    v:        Breakpoint
  ): F[Unit] =
    // Set the observer after the breakpoints are set to do optimistic updates on the UI
    executeEngine.offer(Event.breakpoints(obsId, user, steps, v)) *>
      setObserver(obsId, user, observer)

  override def setOperator(user: User, name: Operator): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting Operator name to '$name' by ${user.displayName}") *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState.operator[F].replace(name.some) >>> refreshSequences)
              .withEvent(SetOperator(name, user.some))

  private def setObserver(
    id:       Observation.Id,
    observer: Observer,
    event:    SeqEvent = SeqEvent.NullSeqEvent
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.modifyState:
      EngineState
        .atSequence[F](id)
        .andThen(SequenceData.observer)
        .replace(observer.some)
        .withEvent(event)

  override def setObserver(
    obsId: Observation.Id,
    user:  User,
    name:  Observer
  ): F[Unit] =
    logDebugEvent(
      s"ObserveEngine: Setting Observer name to '$name' for sequence '$obsId' by ${user.displayName}"
    ) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (
              EngineState
                .atSequence(obsId)
                .modify(SequenceData.observer.replace(name.some)) >>>
                refreshSequence(obsId)
            ).withEvent(SetObserver(obsId, user.some, name))

  override def loadSequence(
    i:        Instrument,
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    val author = s", by '${user.displayName}' on client ${clientId.value}."
    executeEngine.inject(
      // We want the acquisition sequence to reset whenever we load the observation.
      systems.odb.resetAcquisition(obsId) >>
        systems.odb
          .read(obsId)
          .map: odbData =>
            val (errs, stepGen): (List[Throwable], Option[StepGen[F]]) =
              translator.nextStep(odbData, SequenceType.Acquisition.asLeft)
            (errs, odbData, stepGen)
          .attempt
          .flatMap(
            _.fold(
              e =>
                L.warn(e)(s"Error loading observation $obsId$author")
                  .as(
                    Event.pure(
                      SeqEvent.NotifyUser(
                        Notification.LoadingFailed(
                          obsId,
                          List(s"Error loading observation $obsId", e.getMessage)
                        ),
                        clientId
                      )
                    )
                  ),
              { case (errs, odbData, stepGen) =>
                errs.isEmpty
                  .fold(
                    info"Loaded observation $obsId$author",
                    warn"Loaded observation $obsId with warnings: ${errs.mkString}$author"
                  )
                  .as(
                    Event.modifyState {
                      EngineHandle.modifyState { (st: EngineState[F]) =>
                        val instrumentSequenceLens = EngineState.instrumentLoaded[F](i)
                        if (
                          instrumentSequenceLens.get(st).forall(s => executeEngine.canUnload(s.seq))
                        ) {
                          (st.sequences
                             .get(obsId)
                             .fold(
                               ODBSequencesLoader
                                 .loadSequenceMod(
                                   observer.some,
                                   odbData,
                                   instrumentSequenceLens
                                 )
                             )(_ =>
                               ODBSequencesLoader.reloadSequenceMod(
                                 stepGen,
                                 instrumentSequenceLens
                               )
                             )(st),
                           LoadSequence(obsId, clientId)
                          )
                        } else {
                          (
                            st,
                            SeqEvent
                              .NotifyUser(
                                Notification.LoadingFailed(
                                  obsId,
                                  List(
                                    s"Error loading observation $obsId",
                                    s"A sequence is running on instrument $i"
                                  )
                                ),
                                clientId
                              ): SeqEvent
                          )
                        }
                      }
                    }
                  )
              }
            )
          )
    )

  private def logDebugEvent(msg: String): F[Unit] = L.debug(msg)

  private def logInfoEvent(msg: String): F[Unit] = L.info(msg)

  private def logDebugEvent(msg: String, user: User, clientId: ClientId): F[Unit] =
    L.debug(s"$msg, by ${user.displayName} from client $clientId")

  override def clearLoadedSequences(user: User): F[Unit] =
    logDebugEvent("ObserveEngine: Updating loaded sequences") *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            EngineState
              .selected[F]
              .replace(Selected.none)
              .withEvent(ClearLoadedSequences(user.some))

  override def resetConditions: F[Unit] = logDebugEvent("ObserveEngine: Reset conditions") *>
    executeEngine.offer:
      Event.modifyState:
        EngineHandle.modifyState:
          (EngineState.conditions[F].replace(CurrentConditions.Default) >>> refreshSequences)
            .withEvent(SetConditions(CurrentConditions.Default, None))

  override def setConditions(
    conditions: CurrentConditions,
    user:       User
  ): F[Unit] =
    logDebugEvent("ObserveEngine: Setting conditions") *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState.conditions[F].replace(conditions) >>> refreshSequences)
              .withEvent(SetConditions(conditions, user.some))

  override def setImageQuality(iq: ImageQuality, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting image quality to $iq", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.iq)
              .replace(iq.some) >>> refreshSequences)
              .withEvent(SetImageQuality(iq, user.some))

  override def setWaterVapor(wv: WaterVapor, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting water vapor to $wv", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.wv)
              .replace(wv.some) >>> refreshSequences)
              .withEvent(SetWaterVapor(wv, user.some))

  override def setSkyBackground(sb: SkyBackground, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting sky background to $sb", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.sb)
              .replace(sb.some) >>> refreshSequences)
              .withEvent(SetSkyBackground(sb, user.some))

  override def setCloudExtinction(ce: CloudExtinction, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting cloud cover to $ce", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.ce)
              .replace(ce.some) >>> refreshSequences)
              .withEvent(SetCloudExtinction(ce, user.some))

  override def requestRefresh(clientId: ClientId): F[Unit] =
    executeEngine.offer(Event.poll(clientId))

  private val heartbeatPeriod: FiniteDuration = FiniteDuration(10, TimeUnit.SECONDS)

  private def heartbeatStream: Stream[F, Event[F]] = {
    // If there is no heartbeat in 5 periods throw an error
    val noHeartbeatDetection =
      ObserveEngine.failIfNoEmitsWithin[F, Event[F]](
        50 * heartbeatPeriod, // TODO REVERT TO 5 * heartbeatPeriod
        "Engine heartbeat not detected"
      )
    Stream
      .awakeDelay[F](heartbeatPeriod)
      .as(Event.nullEvent: Event[F])
      .through(noHeartbeatDetection.andThen(_.recoverWith { case _ =>
        Stream.eval[F, Event[F]](Event.logErrorMsgF("Observe engine heartbeat undetected"))
      }))
  }

  private def singleActionClientEvent(
    c:            ActionCoords,
    qState:       EngineState[F],
    clientAction: ClientEvent.SingleActionState,
    errorMsg:     Option[String] = none
  ): Option[TargetedClientEvent] =
    qState.sequences
      .get(c.obsId)
      .flatMap(_.resourceAtCoords(c.actCoords))
      .map(res => SingleActionEvent(c.obsId, c.actCoords.stepId, res, clientAction, errorMsg))

  private def viewSequence(obsSeq: SequenceData[F]): SequenceView = {
    val sequenceState: SequenceState[F] = obsSeq.seq
    val instrument: Instrument          = obsSeq.instrument
    val seqType: SequenceType           = sequenceState.currentSequenceType

    val engineLoadedStep: Option[ObserveStep] =
      sequenceState.loadedStep.map: ls =>
        val stepResources = ls.resources.toList.map{ x => (x, sequenceState.getSingleState(ConfigActionCoords(ls.id, x)).actionStatus) }

        StepsView
          .stepsView(instrument)
          .stepView(
            ls.stepGen,
            ls.executionZipper.toEngineStep,
            stepResources,
            obsSeq.pendingObsCmd
          )

    val engStep: Option[ObserveStep] = engineLoadedStep

    // TODO: Implement willStopIn
    SequenceView(
      sequenceState.obsId,
      SequenceMetadata(instrument, obsSeq.observer), // , obsSeq.obsData.title),
      sequenceState.status,
      obsSeq.overrides,
      seqType,
      engStep,
      None,
      sequenceState.breakpoints.value
    )
  }

  private def executionQueueViews(
    st: EngineState[F]
  ): SortedMap[QueueId, ExecutionQueueView] =
    SortedMap(st.queues.map { case (qid, q) =>
      qid -> ExecutionQueueView(qid, q.name, q.cmdState, q.status, q.queue.map(_.obsId))
    }.toList*)

  private def buildObserveStateStream(
    svs:      => SequencesQueue[SequenceView],
    odbProxy: OdbProxy[F]
  ): Stream[F, TargetedClientEvent] =
    Stream.eval:
      odbProxy.getCurrentRecordedIds.map: recordedIds =>
        ObserveState.fromSequenceViewQueue(svs, recordedIds)

  private def toClientModifyEventStream(
    v:        SeqEvent,
    svs:      => SequencesQueue[SequenceView],
    odbProxy: OdbProxy[F]
  ): Stream[F, TargetedClientEvent] =
    v match
      case RequestConfirmation(c @ UserPrompt.ChecksOverride(_, _, _), clientId) =>
        Stream.emit(ClientEvent.ChecksOverrideEvent(c).forClient(clientId))
      // case RequestConfirmation(m, clientId)        => Stream.emit(UserPromptNotification(m, clientId))
      case StartSysConfig(oid, stepId, res)                                      =>
        Stream.emit[F, TargetedClientEvent](
          SingleActionEvent(oid, stepId, res, ClientEvent.SingleActionState.Started, none)
        ) ++ buildObserveStateStream(svs, odbProxy)
      case Busy(obsId, clientId)                                                 =>
        Stream.emit:
          ClientEvent
            .UserNotification(Notification.ResourceConflict(obsId))
            .forClient(clientId)
      case LoadSequence(obsId, clientId)                                         =>
        buildObserveStateStream(svs, odbProxy) ++
          svs.loaded
            .collectFirst:
              case (instrument, oId) if oId === obsId => instrument
            .foldMap: instrument =>
              Stream.emit:
                ClientEvent.ObsLoaded(instrument).forClient(clientId)
      case ResourceBusy(obsId, stepId, resource, clientId)                       =>
        Stream.emit:
          UserNotification(Notification.SubsystemBusy(obsId, stepId, resource)).forClient(clientId)
      case NewStepLoaded(obsId, sequenceType, atomId, stepId)                    =>
        Stream.emit[F, TargetedClientEvent](
          ClientEvent.StepLoaded(obsId, sequenceType, atomId, stepId)
        ) ++
          buildObserveStateStream(svs, odbProxy)
      case AcquisitionCompleted(obsId)                                           =>
        Stream.emit[F, TargetedClientEvent](ClientEvent.AcquisitionPromptReached(obsId)) ++
          buildObserveStateStream(svs, odbProxy)
      case e if e.isModelUpdate                                                  =>
        buildObserveStateStream(svs, odbProxy)
      case _                                                                     => Stream.empty

  private def toClientEvent(
    ev:       EventResult,
    qState:   EngineState[F],
    odbProxy: OdbProxy[F]
  ): Stream[F, TargetedClientEvent] = {
    val sequences: List[SequenceView] =
      qState.sequences.view.values.map(viewSequence).toList

    // Building the view is a relatively expensive operation
    // By putting it into a def we only incur that cost if the message requires it
    def svs: SequencesQueue[SequenceView] =
      SequencesQueue(
        List(
          qState.selected.gmosSouth.map(x => Instrument.GmosSouth -> x.obsId),
          qState.selected.gmosNorth.map(x => Instrument.GmosNorth -> x.obsId),
          qState.selected.ghost.map(x => Instrument.Ghost -> x.obsId),
          qState.selected.flamingos2.map(x => Instrument.Flamingos2 -> x.obsId),
          qState.selected.igrins2.map(x => Instrument.Igrins2 -> x.obsId),
          qState.selected.gnirs.map(x => Instrument.Gnirs -> x.obsId)
        ).flattenOption.toMap,
        qState.conditions,
        qState.operator,
        executionQueueViews(qState),
        sequences
      )

    ev match
      case UserCommandResponse(ue, _, uev) =>
        ue match
          case UserEvent.Pure(NotifyUser(m, cid)) =>
            Stream.emit(UserNotification(m).forClient(cid))
          case UserEvent.ModifyState(_)           =>
            toClientModifyEventStream(uev.getOrElse(NullSeqEvent), svs, odbProxy)
          case e if e.isModelUpdate               => buildObserveStateStream(svs, odbProxy)
          case UserEvent.LogInfo(m, ts)           =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Info, ts, m)))
          case UserEvent.LogWarning(m, ts)        =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Warning, ts, m)))
          case UserEvent.LogError(m, ts)          =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Error, ts, m)))
          case UserEvent.LogDebug(m, ts)          =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Debug, ts, m)))
          case _                                  => Stream.empty
      case SystemUpdate(se, _)             =>
        se match
          // TODO: Sequence completed event not emitted by engine.
          case SystemEvent.PartialResult(i, s, _, Partial(ObsProgress(t, r, v)))   =>
            Stream.emit(
              ProgressEvent(ObservationProgress(i, StepProgress.Regular(s, t, r.value, v)))
            )
          case SystemEvent.PartialResult(i, s, _, Partial(NsProgress(t, r, v, u))) =>
            Stream.emit(
              ProgressEvent(ObservationProgress(i, StepProgress.NodAndShuffle(s, t, r.value, v, u)))
            )
          // case SystemEvent.Busy(id, clientId)                                       =>
          //   Stream.emit(UserNotification(ResourceConflict(id), clientId))
          case SystemEvent.SingleRunCompleted(c, _)                                =>
            Stream.emits(
              singleActionClientEvent(
                c,
                qState,
                ClientEvent.SingleActionState.Completed
              ).toList
            ) ++ buildObserveStateStream(svs, odbProxy)
          case SystemEvent.SingleRunFailed(c, Result.Error(msg))                   =>
            Stream.emits(
              singleActionClientEvent(
                c,
                qState,
                ClientEvent.SingleActionState.Failed,
                msg.some
              ).toList
            ) ++
              Stream.emit(SequenceFailed(c.obsId, msg): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case SystemEvent.StepComplete(obsId)                                     =>
            Stream.emit(StepComplete(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case SystemEvent.SequencePaused(obsId)                                   =>
            Stream.emit(SequencePaused(obsId): TargetedClientEvent)
          case SystemEvent.BreakpointReached(obsId)                                =>
            Stream.emit(BreakpointReached(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case SystemEvent.SequenceComplete(obsId)                                 =>
            Stream.emit(ClientEvent.SequenceComplete(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case SystemEvent.Failed(obsId, _, _, Result.Error(msg))                  =>
            Stream.emit(SequenceFailed(obsId, msg): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case SystemEvent.LoadFailed(obsId, _, Result.Error(msg))                 =>
            Stream.emit(SequenceFailed(obsId, msg): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case e if e.isModelUpdate                                                =>
            buildObserveStateStream(svs, odbProxy)
          case _                                                                   => Stream.empty
  }

  override val clientEventStream: Stream[F, TargetedClientEvent] =
    heartbeatStream
      .as[TargetedClientEvent](BaDum)
      .merge:
        eventResultStream(EngineState.default[F])
          .flatMap: (result, qState) =>
            toClientEvent(result, qState, systems.odb).merge:
              Stream
                .eval:
                  notifyODB(result, qState)
                    .as(none)
                    .handleErrorWith: e =>
                      LogMessage
                        .now(ObserveLogLevel.Error, s"Error notifying ODB: ${e.getMessage}")
                        .map(LogEvent(_): TargetedClientEvent)
                        .map(_.some)
                .unNone
      .merge:
        // Broadcast the current guide configuration whenever it changes.
        systems.guideDb.discrete.map: gcs =>
          ClientEvent.GuideConfigEvent(gcs.config): TargetedClientEvent

  override def eventResultStream(s0: EngineState[F]): Stream[F, (EventResult, EngineState[F])] =
    // TODO We are never using the process function. Consider removing the `process` method and just returning the stream.
    executeEngine.process(PartialFunction.empty)(s0)

  override def stopObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Stop requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(
        Event.getState: engineState =>
          EngineState
            .atSequence(obsId)
            .andThen(SequenceData.seq)
            .getOption(engineState)
            .flatMap(_.loadedStep)
            .foldMap(step => Stream.eval(systems.odb.stepStop(obsId, step.id)))
            .as(Event.nullEvent)
      ) *>
      executeEngine
        .offer(Event.modifyState(setObsCmd(obsId, StopGracefully)))
        .whenA(graceful) *>
      executeEngine.offer:
        Event.actionStop(obsId, translator.stopObserve(obsId, graceful))

  override def abortObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Abort requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.actionStop(obsId, translator.abortObserve(obsId))

  override def pauseObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Pause requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine
        .offer:
          Event.modifyState(setObsCmd(obsId, PauseGracefully))
        .whenA(graceful) *>
      executeEngine.offer:
        Event.actionStop(obsId, translator.pauseObserve(obsId, graceful))

  override def resumeObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Continue requested by ${user.displayName}") *>
      executeEngine.offer(Event.modifyState(clearObsCmd(obsId))) *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.getState(translator.resumePaused(obsId))

  private def queueO(qid: QueueId): Optional[EngineState[F], ExecutionQueue] =
    Focus[EngineState[F]](_.queues).andThen(mapIndex[QueueId, ExecutionQueue].index(qid))

  override def addSequencesToQueue(
    qid:    QueueId,
    obsIds: List[Observation.Id]
  ): F[Unit] = Applicative[F].unit

  override def addSequenceToQueue(
    qid:   QueueId,
    obsId: Observation.Id
  ): F[Unit] = Applicative[F].unit

  override def removeSequenceFromQueue(
    qid:   QueueId,
    obsId: Observation.Id
  ): F[Unit] = Applicative[F].unit

  override def moveSequenceInQueue(
    qid:   QueueId,
    obsId: Observation.Id,
    delta: Int,
    cid:   ClientId
  ): F[Unit] = Applicative[F].unit

  private def clearQ(qid: QueueId): Endo[EngineState[F]] = st =>
    st.queues
      .get(qid)
      .filter(_.status =!= BatchExecState.Running)
      .map { _ =>
        queueO(qid).modify(_.clear)(st)
      }
      .getOrElse(st)

  override def clearQueue(qid: QueueId): F[Unit] =
    executeEngine.offer:
      Event.modifyState:
        EngineHandle.modifyState:
          clearQ(qid).withEvent(UpdateQueueClear(qid))

  override def startQueue(
    qid:      QueueId,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit] = Applicative[F].unit

  override def stopQueue(qid: QueueId, clientId: ClientId): F[Unit] =
    Applicative[F].unit

  private def configSystemCheck(
    sys: Subsystem,
    st:  EngineState[F]
  ): Boolean = {
    // Resources used by running sequences
    val used = ObserveEngine.resourcesInUse(st)

    // Resources reserved by running queues, excluding `sid` to prevent self blocking
//      val reservedByQueues = resourcesReserved(EngineState.sequences[F].modify(_ - sid)(st))
//
//      !(used ++ reservedByQueues).contains(sys)
    !used.contains(sys)
  }

  private def configSystemHandle(
    obsId:    Observation.Id,
    stepId:   Step.Id,
    sys:      Subsystem,
    clientId: ClientId
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap { st0 =>
      if (configSystemCheck(sys, st0))
        st0.sequences.get(obsId).map{ obsData =>
          EngineHandle.liftF(systems.odb.readExecutionConfig(obsId).attempt.map(_.toOption)).flatMap { exec =>
            (for {
              x <- exec
              r <- translator.nextStep(OdbObservationData(obsData.observation, x), stepId.asRight)._2
              act <- r.generator.configs.get(sys).map(_(obsData.overrides))
            } yield executeEngine.startSingle(ConfigActionCoords(stepId, sys), obsData.seq, act).map[SeqEvent] {
              case EventResult.Outcome.Ok => StartSysConfig(obsId, stepId, sys)
              case _ => NullSeqEvent
            }).getOrElse(EngineHandle.pure(NullSeqEvent))
          }
      }.getOrElse(EngineHandle.pure(NullSeqEvent))
      else EngineHandle.pure(ResourceBusy(obsId, stepId, sys, clientId))
    }

//        ObserveEngine                          // Load new step and reload state
//          .retrieveStep(systems.odb, translator, obsId, stepId.asRight)
//          .flatMap(_ => EngineHandle.getState) // A new step may have loaded, so reload state
//          .flatMap: st =>
//            st.sequences
//              .get(obsId)
//              .flatMap(_.configActionCoord(stepId, sys))
//              .map: c =>
//                executeEngine
//                  .startSingle(ActionCoords(obsId, c))
//                  .map[SeqEvent]:
//                    case EventResult.Outcome.Ok => StartSysConfig(obsId, stepId, sys)
//                    case _                      => NullSeqEvent
//              .getOrElse:
//                EngineHandle.pure(NullSeqEvent)
//      } else EngineHandle.pure(ResourceBusy(obsId, stepId, sys, clientId))
//    }

    // EngineHandle.getState.flatMap { st0 =>
    //   if (configSystemCheck(sys, st0))
    //     ObserveEngine.loadStep(systems.odb, translator, obsId, stepId.asRight) >>
    //       // We have to reread the state after loading
    //       EngineHandle.getState.flatMap { st =>
    //         st.sequences
    //           .get(obsId)
    //           .flatMap(_.configActionCoord(stepId, sys))
    //           .map: c =>
    //             executeEngine
    //               .startSingle(ActionCoords(obsId, c))
    //               .map[SeqEvent]:
    //                 case EventResult.Outcome.Ok => StartSysConfig(obsId, stepId, sys)
    //                 case _                      => NullSeqEvent
    //           .getOrElse(
    //             EngineHandle.pure(NullSeqEvent)
    //           )
    //       }
    //   else
    //     EngineHandle.pure(ResourceBusy(obsId, stepId, sys, clientId))
    // }

  /**
   * Triggers the application of a specific step configuration to a system
   */
  override def configSystem(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    stepId:   Step.Id,
    sys:      Subsystem,
    clientId: ClientId
  ): F[Unit] =
    setObserver(obsId, user, observer) >>
      executeEngine.offer:
        Event.modifyState:
          configSystemHandle(obsId, stepId, sys, clientId)

  private def notifyOdbSequencePaused(obsId: Observation.Id): F[Unit] =
    systems.odb
      .obsPause(obsId)
      .ensure(
        ObserveFailure
          .Unexpected("Unable to send ObservationPaused message to ODB.")
      )(identity)
      .void

  def notifyODB(
    i: (EventResult, EngineState[F])
  ): F[(EventResult, EngineState[F])] =
    (i match {
      case (SystemUpdate(SystemEvent.Failed(obsId, stepId, _, e), _), _)         =>
        Logger[F].error(s"Error executing $obsId due to $e") <*
          systems.odb
            .stepAbort(obsId, stepId)
            .ensure(
              ObserveFailure
                .Unexpected("Unable to send ObservationAborted message to ODB.")
            )(identity)
      case (SystemUpdate(SystemEvent.SequencePaused(obsId), _), _)               =>
        notifyOdbSequencePaused(obsId)
      case (SystemUpdate(SystemEvent.BreakpointReached(obsId), _), _)            =>
        notifyOdbSequencePaused(obsId)
      case (SystemUpdate(SystemEvent.LoadFailed(obsId, _, e), _), _)             =>
        Logger[F].error(s"Error loading $obsId due to $e") <*
          systems.odb
            .obsStop(obsId)
            .ensure(
              ObserveFailure
                .Unexpected("Unable to send ObservationLoadFailed message to ODB.")
            )(identity)
      case (UserCommandResponse(UserEvent.ModifyState(_), _, Some(seqEvent)), _) =>
        seqEvent match
          case AcquisitionCompleted(obsId)                        =>
            notifyOdbSequencePaused(obsId)
          case NewStepLoaded(obsId, sequenceType, atomId, stepId) =>
            systems.odb
              .obsContinue(obsId)
              .ensure(
                ObserveFailure
                  .Unexpected("Unable to send ObservationContinued message to ODB.")
              )(identity)
              .void
          case _                                                  =>
            Applicative[F].unit
      case _                                                                     => Applicative[F].unit
    }).as(i)

  private def updateSequenceEndo(
    conditions: CurrentConditions,
    operator:   Option[Operator]
  ): Endo[SequenceData[F]] =
    (sd: SequenceData[F]) =>
      val newStep: Option[EngineStep[F]] =
        sd.loadedStep.map: ls =>
          generateStep(
            ls.stepGen,
            sd.overrides,
            HeaderExtraData(conditions, operator, sd.observer)
          )._1

      SequenceData.seq.modify(
        executeEngine.updateStep(newStep)
      )(sd)

  private def refreshSequence(id: Observation.Id): Endo[EngineState[F]] = (st: EngineState[F]) =>
    EngineState.atSequence(id).modify(updateSequenceEndo(st.conditions, st.operator))(st)

  private def refreshSequences: Endo[EngineState[F]] = (st: EngineState[F]) =>
    val f: Endo[EngineState[F]] =
      List(
        EngineState.gmosNorthSequence[F],
        EngineState.gmosSouthSequence[F],
        EngineState.flamingos2Sequence[F],
        EngineState.igrins2Sequence[F],
        EngineState.gnirsSequence[F]
      ).map(_.modify(updateSequenceEndo(st.conditions, st.operator))).combineAll
    f(st)

  private def toggleOverride(
    resource: String,
    modify:   (SubsystemEnabled, SystemOverrides) => SystemOverrides,
    event:    SeqEvent,
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    logDebugEvent(
      s"ObserveEngine: Setting $resource enabled flag to '$enabled' for sequence '$obsId'",
      user,
      clientId
    ) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (
              EngineState
                .atSequence(obsId)
                .modify(SequenceData.overrides.modify(x => modify(enabled, x)))
                >>> refreshSequence(obsId)
            ).withEvent(event)

  override def setTcsEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      Resource.TCS.label,
      (enabled, x) => if (enabled.value) x.enableTcs else x.disableTcs,
      SetTcsEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId
    )

  override def setGcalEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      Resource.Gcal.label,
      (enabled, x) => if (enabled.value) x.enableGcal else x.disableGcal,
      SetGcalEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId
    )

  override def setInstrumentEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      "Instrument",
      (enabled, x) => if (enabled.value) x.enableInstrument else x.disableInstrument,
      SetInstrumentEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId
    )

  override def setDhsEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      "DHS",
      (enabled, x) => if (enabled.value) x.enableDhs else x.disableDhs,
      SetDhsEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId: ClientId
    )

}
