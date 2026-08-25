// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.Endo
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import monocle.Optional
import mouse.boolean.*
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.SequenceStatus.*
import observe.server.EngineState
import observe.server.SeqEvent
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.Tracer

import EventResult.Outcome
import EventResult.SystemUpdate
import EventResult.UserCommandResponse
import Result.PartialVal
import Result.RetVal
import UserEvent.*
import Handle.given

/**
 * An [[Event]] paired with the trace context that was active when it was enqueued. The engine
 * processes events in a single background fiber started at server startup. TracedEvent can capture
 * the trace id of the parent context on enqueue and use to follow the trace all the way to the odb.
 */
private[engine] final case class TracedEvent[F[_]](
  traceParent: Option[SpanContext],
  event:       Event[F]
)

class Engine[F[_]: {MonadCancelThrow, Logger, Tracer as T}] private (
  streamQueue:  Queue[F, Stream[F, TracedEvent[F]]],
  inputQueue:   Queue[F, TracedEvent[F]],
  loadNextStep: (Engine[F], Observation.Id, SequenceType, Atom.Id) => EngineHandle[F, SeqEvent]
) {
  val L: Logger[F] = Logger[F]

  private def setObsStatus(obsId: Observation.Id)(st: SequenceStatus): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(obsId)(SequenceState.status.replace(st))

  private def cleanLoadedStep(obsId: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(obsId)(_.withNoLoadedStep)

  def pause(id: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(id)(SequenceState.userStopSet(HasUserStop.Yes))

  private def cancelPause(id: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(id)(SequenceState.userStopSet(HasUserStop.No))

  def startSingle(c: ConfigActionCoords, seq: SequenceState[F], act: Action[F]): EngineHandle[F, Outcome] = {
    val resultStream: Option[Stream[F, Result]] =
      ((seq.status.isIdle || seq.status.isError) && !seq.getSingleState(c).active).option(act.gen)

    resultStream
      .map { p =>
        EngineHandle.modifySequenceState[F](seq.obsId)(u => u.startSingle(c)) *>
          EngineHandle
            .fromEventStream(
              p.attempt.flatMap {
                case Right(r @ Result.OK(_))    =>
                  Stream.emit(Event.singleRunCompleted(c, r))
                case Right(e @ Result.Error(_)) =>
                  Stream.emit(Event.singleRunFailed(c, e))
                case Right(r)                   =>
                  Stream.emit(
                    Event.singleRunFailed(
                      c,
                      Result.Error(s"Unhandled result for single run action: $r")
                    )
                  )
                case Left(t: Throwable)         => Stream.raiseError[F](t)
              }
            )
            .as[Outcome](Outcome.Ok)
      }
      .getOrElse(EngineHandle.pure(Outcome.Failure))
    }

  private def completeSingleRun[V <: RetVal](c: ConfigActionCoords, r: V): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(c.obsId)(_.completeSingle(c, r))

  private def failSingleRun(c: ConfigActionCoords, e: Result.Error): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(c.obsId)(_.failSingle(c, e))

  /**
   * Tells if a sequence can be safely removed
   */
  def canUnload(obsId: Observation.Id)(st: EngineState[F]): Boolean =
    EngineState.sequenceStateAt(obsId).getOption(st).forall(canUnload)

  def canUnload(seq: SequenceState[F]): Boolean = SequenceState.canUnload(seq)

  /**
   * Refresh the steps executions of an existing sequence. Does not add nor remove steps.
   * @param id
   *   sequence identifier
   * @param steps
   *   List of new steps definitions
   * @return
   */
  def update(obsId: Observation.Id, step: Option[EngineStep[F]]): Endo[EngineState[F]] =
    EngineState.sequenceStateAt(obsId).modify(_.update(step.map(_.executions)))

  def updateStep(step: Option[EngineStep[F]]): Endo[SequenceState[F]] =
    _.update(step.map(_.executions))

  /**
   * Adds the current `Execution` to the completed `Queue`, makes the next pending `Execution` the
   * current one, and initiates the actual execution.
   *
   * If there are no more pending `Execution`s, it emits the `Finished` event.
   */
  private def nextExecution(obsId: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(seqState =>
        seqState
          .map: seq =>
            (seq.status, seq.loadedStep) match {
              case (SequenceStatus.Running(userStop, internalStop, _, _, _), Some(completedStep)) =>
                seq.withNextExecution match {
                  // Empty state, should never happen
                  case None                                            =>
                    send(Event.sequenceComplete(obsId))
                  // Step completed (no more execution groups - loadedStep is cleared by `withNextExecution`)
                  case Some(nextState) if nextState.loadedStep.isEmpty =>
                    EngineHandle.replaceSequenceState(obsId)(nextState) *>
                      (if (userStop || internalStop)
                         setObsStatus(obsId)(SequenceStatus.Idle) *>
                           send(Event.sequencePaused(obsId))
                       else
                         send(Event.stepComplete(obsId)) >>
                           send(
                             Event.modifyState(
                               loadNextStep(
                                 this,
                                 obsId,
                                 seq.currentSequenceType,
                                 completedStep.atomId
                               )
                             )
                           ))
                  // Execution group completed. Check requested stop and breakpoint.
                  case Some(nextState)                                 =>
                    EngineHandle.replaceSequenceState(obsId)(nextState) *>
                      (if (
                         nextState.getCurrentBreakpoint &&
                         !nextState.currentExecution.execution.exists(_.uninterruptible)
                       ) {
                         setObsStatus(obsId)(SequenceStatus.Idle) *>
                           cleanLoadedStep(obsId) *>
                           send(Event.breakpointReached(obsId))
                       } else send(Event.executing(obsId)))
                }
              case _                                                                              => EngineHandle.unit
            }
          .getOrElse(EngineHandle.unit)
      )

  def startLoadedStep(obsId: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(seqState =>
        seqState
          .map { seq =>
            seq.status match {
              case SequenceStatus
                    .Running(userStop, internalStop, _, _, isStarting) =>
                // TODO Review if all of these conditions are possible with new sequence flow.
                if (!isStarting && (userStop || internalStop)) {
                  if (seq.loadedStep.isEmpty)
                    send(Event.sequenceComplete(obsId))
                  else
                    setObsStatus(obsId)(SequenceStatus.Idle)
                } else {
                  if (seq.loadedStep.isEmpty)
                    send(Event.sequenceComplete(obsId))
                  else if (!isStarting && seq.getCurrentBreakpoint)
                    setObsStatus(obsId)(SequenceStatus.Idle) *> send(
                      Event.breakpointReached(obsId)
                    )
                  else
                    setObsStatus(obsId)(
                      SequenceStatus.Running(
                        userStop,
                        internalStop,
                        IsWaitingUserPrompt.No,
                        IsWaitingNextStep.No,
                        IsStarting.No
                      )
                    ) *>
                      send(Event.executing(obsId))
                }
              case _ => EngineHandle.unit
            }
          }
          .getOrElse(EngineHandle.unit)
      )

  /**
   * Executes all actions in the `Current` `Execution` in parallel. When all are done it emits the
   * `Executed` event. It also updates the `State` as needed.
   */
  // Send the expected event when the `Action` is executed
  // It doesn't catch run time exceptions. If desired, the Action has to do it itself.
  private def act(
    id:     Observation.Id,
    stepId: Step.Id,
    t:      (Stream[F, Result], Int)
  ): Stream[F, Event[F]] = t match {
    case (gen, i) =>
      gen
        .takeThrough:
          case Result.Partial(_) => true
          case _                 => false
        .attempt
        .flatMap:
          case Right(r @ Result.OK(_))        => Stream.emit(Event.completed(id, stepId, i, r))
          case Right(r @ Result.OKStopped(_)) => Stream.emit(Event.stopCompleted(id, stepId, i, r))
          case Right(r @ Result.OKAborted(_)) => Stream.emit(Event.aborted(id, stepId, i, r))
          case Right(r @ Result.Partial(_))   => Stream.emit(Event.partial(id, stepId, i, r))
          case Right(e @ Result.Error(_))     => Stream.emit(Event.failed(id, stepId, i, e))
          case Right(r @ Result.Paused(_))    => Stream.emit(Event.paused(id, stepId, i, r))
          case Left(t: Throwable)             => Stream.raiseError[F](t)
  }

  private def executeLoadedStep(obsId: Observation.Id)(using Concurrent[F]): EngineHandle[F, Unit] =
    EngineHandle.getState.flatMap(st =>
      EngineState
        .sequenceStateAt(obsId)
        .getOption(st)
        .map: seq =>
          seq.loadedStep match
            case None                  =>
              // No step executing — sequence is done
              EngineHandle.replaceSequenceState(obsId)(seq) >>
                send(Event.sequenceComplete(obsId))
            case Some(executionZipper) =>
              val stepId: Step.Id                                   = executionZipper.id
              val eventStreams: List[Stream[F, Event[F]]]           =
                seq.currentExecution.actions
                  .map(_.gen)
                  .zipWithIndex
                  .map(act(obsId, stepId, _))
              val mergedEventStream: Stream[F, Event[F]]            =
                Stream.emits(eventStreams).parJoin(eventStreams.length)
              val setExecutionsStarted: List[EngineHandle[F, Unit]] =
                seq.currentExecution.actions.indices
                  .map(i => EngineHandle.modifySequenceState[F](obsId)(_.start(i)))
                  .toList
              setExecutionsStarted.sequence >> Handle.fromEventStream(mergedEventStream)
        .getOrElse(EngineHandle.unit)
    )

  private def actionStop(
    obsId: Observation.Id,
    f:     EngineState[F] => Stream[F, Event[F]]
  ): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(_.map { s =>
        (EngineHandle.fromEventStream(f) >>
          EngineHandle.modifySequenceState(obsId)(
            SequenceState.internalStopSet(HasInternalStop.Yes)
          ))
          .whenA(SequenceState.isRunning(s))
      }.getOrElse(EngineHandle.unit))

  /**
   * Given the index of the completed `Action` in the current `Execution`, it marks the `Action` as
   * completed and returns the new updated `State`.
   *
   * When the index doesn't exist it does nothing.
   */
  private def complete[R <: RetVal](
    obsId: Observation.Id,
    i:     Int,
    r:     Result.OK[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(r)) *>
      EngineHandle
        .getSequenceState(obsId)
        .flatMap(
          _.flatMap(
            _.currentExecution.execution
              .forall(Action.completed)
              .option(EngineHandle.fromSingleEvent(Event.executed(obsId)))
          ).getOrElse(EngineHandle.unit)
        )

  private def stopComplete[R <: RetVal](
    obsId: Observation.Id,
    i:     Int,
    r:     Result.OKStopped[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(r)) *>
      EngineHandle
        .getSequenceState(obsId)
        .flatMap(
          _.flatMap(
            _.currentExecution.execution
              .forall(Action.completed)
              .option(Handle.fromSingleEvent(Event.executed(obsId)))
          ).getOrElse(EngineHandle.unit)
        )

  private def abort[R <: RetVal](
    obsId: Observation.Id,
    i:     Int,
    r:     Result.OKAborted[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(r)) >>
      setObsStatus(obsId)(SequenceStatus.Aborted) >>
      cleanLoadedStep(obsId)

  private def partialResult[R <: PartialVal](
    obsId: Observation.Id,
    i:     Int,
    p:     Result.Partial[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(obsId)(_.mark(i)(p))

  def actionPause(id: Observation.Id, i: Int, p: Result.Paused): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(id)(s =>
      SequenceState.internalStopSet(HasInternalStop.No)(s).mark(i)(p)
    )

  private def actionResume(
    obsId: Observation.Id,
    i:     Int,
    cont:  Stream[F, Result]
  ): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(seqStateOpt =>
        (for
          seqState   <- seqStateOpt if SequenceState.isRunning(seqState) &&
            seqState.currentExecution.execution.lift(i).exists(Action.paused)
          loadedStep <- seqState.loadedStep
        yield EngineHandle.modifySequenceState[F](obsId)(_.start(i)) >>
          EngineHandle.fromEventStream(act(obsId, loadedStep.id, (cont, i))))
          .getOrElse(EngineHandle.unit)
      )

  /**
   * For now it only changes the `Status` to `Paused` and returns the new `State`. In the future
   * this function should handle the failed action.
   */
  private def fail(obsId: Observation.Id)(i: Int, e: Result.Error): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(e)) *>
      setObsStatus(obsId)(SequenceStatus.Failed(e.msg))

  private def logError(e: Result.Error): EngineHandle[F, Unit] = error(e.errMsg.getOrElse(e.msg))

  /**
   * Log info lifted into Handle.
   */
  private def info(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.info(msg))

  /**
   * Log warning lifted into Handle.
   */
  private def warning(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.warn(msg))

  /**
   * Log debug lifted into Handle.
   */
  private def debug(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.debug(msg))

  /**
   * Log error lifted into Handle
   */
  private def error(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.error(msg))

  /**
   * Enqueue `Event` in the Handle.
   */
  private def send(ev: Event[F]): EngineHandle[F, Unit] = Handle.fromEventStream(Stream(ev))

  private def handleUserEvent(ue: UserEvent[F]): EngineHandle[F, EventResult] = ue match {
    case Pause(obsId, _)                   =>
      debug(s"Engine: Pause requested for sequence $obsId") *> pause(obsId) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case CancelPause(obsId, _)             =>
      debug(s"Engine: Pause canceled for sequence $obsId") *> cancelPause(obsId) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Breakpoints(obsId, _, stepIds, v) =>
      debug(s"Engine: breakpoints changed for sequence $obsId and steps $stepIds to $v") *>
        EngineHandle.modifySequenceState[F](obsId)(_.setBreakpoints(stepIds.map(id => (id, v)))) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Poll(_)                           =>
      debug("Engine: Polling current state") *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case GetState(f)                       =>
      EngineHandle.fromEventStream(f) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case ModifyState(f)                    =>
      f.map((r: SeqEvent) => UserCommandResponse[F](ue, Outcome.Ok, Some(r)))
    case ActionStop(obsId, f)              =>
      debug("Engine: Action stop requested") *> actionStop(obsId, f) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case ActionResume(obsId, i, cont)      =>
      debug("Engine: Action resume requested") *> actionResume(obsId, i, cont) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogDebug(msg, _)                  =>
      debug(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogInfo(msg, _)                   =>
      info(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogWarning(msg, _)                =>
      warning(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogError(msg, _)                  =>
      error(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Pure(v)                           =>
      EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, v.some))
  }

  private def handleSystemEvent(
    se: SystemEvent
  )(using Concurrent[F]): EngineHandle[F, EventResult] =
    import SystemEvent.*
    se match {
      case Completed(obsId, _, i, r)     =>
        debug(s"Engine: From sequence $obsId: Action completed ($r)") *>
          complete(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case StopCompleted(obsId, _, i, r) =>
        debug(s"Engine: From sequence $obsId: Action completed with stop ($r)") *>
          stopComplete(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Aborted(obsId, _, i, r)       =>
        debug(s"Engine: From sequence $obsId: Action completed with abort ($r)") *>
          abort(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case PartialResult(obsId, _, i, r) =>
        debug(s"Engine: From sequence $obsId: Partial result ($r)") *>
          partialResult(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Paused(obsId, _, i, r)        =>
        debug("Engine: Action paused") *>
          actionPause(obsId, i, r) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Failed(obsId, _, i, e)        =>
        logError(e) *> fail(obsId)(i, e) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case LoadFailed(obsId, i, e)       =>
        logError(e) *> fail(obsId)(i, e) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Busy(obsId, _)                =>
        warning(
          s"Cannot run sequence $obsId " +
            s"because " +
            s"required systems are in use."
        ) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case BreakpointReached(obsId)      =>
        debug(s"Engine: Breakpoint reached in observation [$obsId]") *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Executed(obsId)               =>
        debug(s"Engine: Execution $obsId completed") *>
          nextExecution(obsId) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Executing(obsId)              =>
        debug("Engine: Executing") *>
          executeLoadedStep(obsId) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case StepComplete(obsId)           =>
        debug(s"Engine: Step completed for observation [$obsId]") *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SequencePaused(obsId)         =>
        debug(s"Engine: Sequence paused for observation [$obsId]") *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SequenceComplete(obsId)       =>
        debug("Engine: Finished") *>
          setObsStatus(obsId)(SequenceStatus.Completed) >>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SingleRunCompleted(c, r)      =>
        debug(s"Engine: single action $c completed with result $r") *>
          completeSingleRun(c, r.response) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SingleRunFailed(c, e)         =>
        debug(s"Engine: single action $c failed with error $e") *>
          failSingleRun(c, e) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Null                          => EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
    }

  /**
   * Main logical thread to handle events and produce output.
   */
  private def run(
    onSystemEvent: PartialFunction[SystemEvent, EngineHandle[F, Unit]]
  )(ev: Event[F])(using Concurrent[F]): EngineHandle[F, EventResult] =
    ev match
      case Event.EventUser(ue)   => handleUserEvent(ue)
      case Event.EventSystem(se) =>
        handleSystemEvent(se).flatMap: (r: EventResult) =>
          onSystemEvent.applyOrElse(se, (_: SystemEvent) => EngineHandle.unit).as(r)

  private def spanForEvent[A](ev: Event[F])(fa: F[A]): F[A] =
    ev match
      case Event.EventUser(ModifyState(_)) => T.span("engine-modify-state").surround(fa)
      case _                               => fa

  /** Traverse a process with a stateful computation. */
  // input, stream of events
  // initalState: state
  // f takes an event and the current state, it produces a new state, a new value B and more actions
  def mapEvalState(
    initialState: EngineState[F],
    f:            (
      Event[F],
      EngineState[F]
    ) => F[(EngineState[F], (EventResult, EngineState[F]), Stream[F, Event[F]])]
  )(using Concurrent[F]): Stream[F, (EventResult, EngineState[F])] =
    Stream.exec(streamQueue.offer(Stream.fromQueueUnterminated(inputQueue))) ++
      Stream
        .fromQueueUnterminated(streamQueue)
        .parJoinUnbounded
        .evalMapAccumulate(initialState): (s, te) =>
          // Restore the trace context captured when the event was enqueued, so the effects
          // executed in this background consumer fiber are parented to the originating request.
          val handled  = spanForEvent(te.event)(f(te.event, s))
          val runEvent =
            te.traceParent.fold(handled)(c => T.childScope(c)(handled))
          runEvent.flatMap:
            // Optimization to avoid processing empty streams.
            case (ns, b, Stream.empty) => (ns, b).pure[F]
            // Tag produced events with the same parent, so that whatever they trigger downstream
            // is still parented to the originating request.

            // TODO `map` only tags the emitted events; it does not scope the effects `st` itself
            // performs when pulled (e.g. the odb calls inside action streams from
            // `executeLoadedStep`).
            case (ns, b, st) =>
              streamQueue.offer(st.map(TracedEvent(te.traceParent, _))) >> (ns, b).pure[F]
        .map(_._2)

  private def runE(
    onSystemEvent: PartialFunction[SystemEvent, EngineHandle[F, Unit]]
  )(ev: Event[F], s: EngineState[F])(using
    ci:            Concurrent[F]
  ): F[(EngineState[F], (EventResult, EngineState[F]), Stream[F, Event[F]])] =
    run(onSystemEvent)(ev).stateT.run(s).map { case (si, (r, p)) =>
      (si, (r, si), p)
    }

  // Only used for testing.
  def process(
    onSystemEvent: PartialFunction[SystemEvent, EngineHandle[F, Unit]]
  )(s0: EngineState[F])(using
    ev:            Concurrent[F]
  ): Stream[F, (EventResult, EngineState[F])] =
    mapEvalState(s0, runE(onSystemEvent)(_, _))

  def offer(in: Event[F]): F[Unit] =
    T.currentSpanContext.flatMap(ctx => inputQueue.offer(TracedEvent(ctx, in)))

  def inject(f: F[Event[F]]): F[Unit] =
    T.currentSpanContext.flatMap: ctx =>
      streamQueue.offer:
        // `f` is evaluated when the stream is pulled, so restore the parent context.
        Stream.eval(ctx.fold(f)(c => T.childScope(c)(f))).map(TracedEvent(ctx, _))
}

object Engine {

  trait State[F[_], D] {
    def sequenceStateIndex(sid: Observation.Id): Optional[D, SequenceState[F]]
  }

  trait Types[S, E] {
    type StateType = S
    type EventData = E
  }

  def build[F[_]: {Concurrent, Logger, Tracer}](
    loadNextStep: (Engine[F], Observation.Id, SequenceType, Atom.Id) => EngineHandle[F, SeqEvent]
  ): F[Engine[F]] = for {
    sq <- Queue.unbounded[F, Stream[F, TracedEvent[F]]]
    iq <- Queue.unbounded[F, TracedEvent[F]]
  } yield new Engine(sq, iq, loadNextStep)

}
