// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import monocle.Focus
import monocle.Lens
import observe.model.SequenceStatus
import observe.model.SequenceStatus.HasInternalStop
import observe.model.SequenceStatus.HasUserStop
import observe.model.SystemOverrides
import observe.server.HeaderExtraData
import observe.server.StepGen
import observe.server.engine.Action.ActionState
import observe.server.engine.Result.RetVal
import observe.server.generateStep

final case class SequenceState[F[_]](
  obsId:               Observation.Id,
  status:              SequenceStatus,
  loadedStep:          Option[LoadedStep[F]], // None = idle/done, Some = executing
  currentSequenceType: SequenceType,
  breakpoints:         Breakpoints,
  singleRuns:          Map[ConfigActionCoords, ActionState]
):
  /**
   * Advances execution within the current step's execution groups. Returns `None` if the step is
   * completed (no more execution groups).
   */
  lazy val withNextExecution: Option[SequenceState[F]] =
    loadedStep match
      case None     => None
      case Some(ls) =>
        val newBreakpoints: Breakpoints = breakpoints - ls.id
        ls.withNextExecution match
          case None        => // Step completed - all execution groups done
            Some(copy(loadedStep = None, breakpoints = newBreakpoints))
          case Some(newLs) => // More execution groups to run
            Some(copy(loadedStep = Some(newLs), breakpoints = newBreakpoints))

  def rollback: SequenceState[F] = copy(loadedStep = loadedStep.map(_.rollback))

  def setBreakpoints(breakpointsDelta: Set[(Step.Id, Breakpoint)]): SequenceState[F] =
    copy(
      breakpoints = breakpointsDelta.foldLeft(breakpoints) { case (accum, (stepId, breakpoint)) =>
        if breakpoint === Breakpoint.Enabled then accum + stepId else accum - stepId
      }
    )

  def getCurrentBreakpoint: Boolean =
    loadedStep.exists(z => breakpoints.contains(z.id) && z.done.isEmpty)

  lazy val currentExecution: Execution[F] =
    loadedStep.map(_.focus).getOrElse(Execution.empty)

  val done: Option[EngineStep[F]] =
    loadedStep.flatMap(_.executionZipper.uncurrentify)

  def mark(i: Int)(r: Result): SequenceState[F] =
    loadedStep match
      case None     => this
      case Some(ls) => copy(loadedStep = Some(ls.mark(i)(r)))

  def start(i: Int): SequenceState[F] =
    loadedStep match
      case None     => this
      case Some(ls) => copy(loadedStep = Some(ls.start(i)), singleRuns = Map.empty)

  def update(stepDef: Option[List[ParallelActions[F]]]): SequenceState[F] =
    (loadedStep, stepDef) match
      case (Some(z), Some(t)) => copy(loadedStep = Some(z.update(t)))
      case _                  => this

  // Functions to handle single run of Actions
  def startSingle(c: ConfigActionCoords): SequenceState[F] =
    loadedStep match
      case Some(z) if z.done.nonEmpty => this // already past initial execution
      case _                          =>
        copy(singleRuns = singleRuns + (c -> ActionState.Started))

  def failSingle(c: ConfigActionCoords, err: Result.Error): SequenceState[F] =
    if getSingleState(c).started
    then copy(singleRuns = singleRuns + (c -> ActionState.Failed(err)))
    else this

  def completeSingle[V <: RetVal](c: ConfigActionCoords, r: V): SequenceState[F] =
    if getSingleState(c).started
    then copy(singleRuns = singleRuns + (c -> ActionState.Completed(r)))
    else this

  def getSingleState(c: ConfigActionCoords): ActionState =
    singleRuns.getOrElse(c, ActionState.Idle)

//  def getSingleAction(c: ActionCoordsInSeq): Option[Action[F]] =
//    for
//      step <- loadedStep.filter(_.id === c.stepId)
//      exec <- step.executionZipper.toEngineStep.executions.get(c.execIdx.value)
//      act  <- exec.get(c.actIdx.value)
//    yield act

  val getSingleActionStates: Map[ConfigActionCoords, ActionState] = singleRuns

  def clearSingles: SequenceState[F] = copy(singleRuns = Map.empty)

  def withLoadedStepGen(
    stepGenOpt:  Option[StepGen[F]],
    overrides:   SystemOverrides,
    headerExtra: HeaderExtraData
  ): SequenceState[F] =
    stepGenOpt.fold(copy(loadedStep = none)): stepGen =>
      val (engineStep, breakpoint) = generateStep(stepGen, overrides, headerExtra)
      ExecutionZipper.currentify(engineStep) match
        case None     => this
        case Some(es) =>
          copy(
            loadedStep = Some(LoadedStep(stepGen = stepGen, executionZipper = es)),
            breakpoints = this.breakpoints.merge(BreakpointsDelta.one(stepGen.id, breakpoint))
          )

  def withNoLoadedStep: SequenceState[F] = copy(loadedStep = none)

  def withIdleStatus: SequenceState[F] = copy(status = SequenceStatus.Idle)

  def withFailedStatus(err: String): SequenceState[F] =
    copy(status = SequenceStatus.Failed(err))

object SequenceState:

  def status[F[_]]: Lens[SequenceState[F], SequenceStatus] =
    Focus[SequenceState[F]](_.status)

  def loadedStep[F[_]]: Lens[SequenceState[F], Option[LoadedStep[F]]] =
    Focus[SequenceState[F]](_.loadedStep)

  def isRunning[F[_]](st: SequenceState[F]): Boolean = st.status.isRunning

  def canUnload[F[_]](st: SequenceState[F]): Boolean = st.status.canUnload

  def userStopRequested[F[_]](st: SequenceState[F]): Boolean = st.status.isUserStopRequested

  def anyStopRequested[F[_]](st: SequenceState[F]): Boolean = st.status match
    case SequenceStatus.Running(u, i, _, _, _) => u || i
    case _                                     => false

  def isWaitingUserPrompt[F[_]](st: SequenceState[F]): Boolean = st.status.isWaitingUserPrompt

  def isStarting[F[_]](st: SequenceState[F]): Boolean = st.status.isStarting

  def userStopSet[F[_]](v: HasUserStop): SequenceState[F] => SequenceState[F] = status.modify {
    case r @ SequenceStatus.Running(_, _, _, _, _) => r.copy(userStop = v)
    case r                                         => r
  }

  def internalStopSet[F[_]](v: HasInternalStop): SequenceState[F] => SequenceState[F] =
    status.modify {
      case r @ SequenceStatus.Running(_, _, _, _, _) => r.copy(internalStop = v)
      case r                                         => r
    }

  /**
   * Initialize a `State` from a single step (EngineStep).
   */
  // def init[F[_]](obsId: Observation.Id, step: EngineStep[F]): SequenceState[F] =
  //   EngineStep.Zipper.currentify(step) match
  //     case Some(z) =>
  //       SequenceState(obsId, SequenceStatus.Idle, Some(z), Breakpoints.empty, Map.empty)
  //     case None    =>
  //       SequenceState(obsId, SequenceStatus.Idle, None, Breakpoints.empty, Map.empty)

  /**
   * Initialize a `State` from a `Sequence`.
   */
  def init[F[_]](
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    breakpoints:  Breakpoints,
    status:       SequenceStatus = SequenceStatus.Idle
  ): SequenceState[F] =
    SequenceState(obsId, status, none, sequenceType, breakpoints, Map.empty)

  /**
   * Rebuilds the state of a sequence with a new steps definition. The sequence must not be running.
   */
  def reload[F[_]](
    stepGenOpt:  Option[StepGen[F]],
    overrides:   SystemOverrides,
    headerExtra: HeaderExtraData,
    oldState:    SequenceState[F]
  ): SequenceState[F] =
    if oldState.status.isRunning
    then oldState
    else oldState.withLoadedStepGen(stepGenOpt, overrides, headerExtra)
