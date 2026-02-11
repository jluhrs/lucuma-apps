// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.*
import lucuma.schemas.model.Visit
import lucuma.schemas.model.enums.AtomExecutionState
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.sequence.*

private trait SequenceTable[S, D]:
  def visits: List[Visit[D]]
  // def config: ExecutionConfig[S, D]
  def staticConfig: S
  def acquisition: Option[Atom[D]]
  def science: Option[List[Atom[D]]]
  def signalToNoise: SequenceType => D => Option[SignalToNoise]
  def isEditing: IsEditing
  def i: Int // TODO This is a temporary mechanism for demo purposes

  private def futureSteps(
    seqType:        SequenceType,
    currentSeqType: Option[SequenceType]
  )(sequence: List[Atom[D]]): List[SequenceRow.FutureStep[D]] =
    val allSteps: List[SequenceRow.FutureStep[D]] =
      SequenceRow.FutureStep
        .fromAtoms(sequence, signalToNoise(seqType), seqType)
    if (currentSeqType.contains_(seqType)) allSteps.tail
    else allSteps
  end futureSteps

  private lazy val currentVisitData: Option[(Visit.Id, SequenceType, Boolean)] =
    // If the last atom of the last visit is Ongoing, the sequence is executing.
    visits.lastOption
      .filter:
        _.atoms.lastOption.exists:
          _.executionState === AtomExecutionState.Ongoing
      .map(visit =>
        (visit.id,
         visit.atoms.last.sequenceType,
         // The sequence is executing if the last step of the last visit is Ongoing.
         // Note: The last atom could be Ongoing but be paused, in which case there's no executing step.
         visit.atoms.lastOption
           .flatMap(_.steps.lastOption)
           .exists(_.executionState === StepExecutionState.Ongoing)
        )
      )

  protected[sequence] lazy val currentVisitId: Option[Visit.Id] =
    currentVisitData.map(_._1)
  private lazy val currentSequenceType: Option[SequenceType]    =
    currentVisitData.map(_._2)
  private lazy val isExecuting: Boolean                         =
    currentVisitData.exists(_._3)

  protected[sequence] lazy val scienceRows: List[SequenceRow[D]] =
    science
      .map(futureSteps(SequenceType.Science, currentSequenceType.filter(_ => isExecuting)))
      .orEmpty

  // Hide acquisition when science is executing or when sequence is complete.
  protected[sequence] lazy val isAcquisitionDisplayed: Boolean =
    !currentSequenceType.contains_(SequenceType.Science) && scienceRows.nonEmpty

  protected[sequence] lazy val acquisitionRows: List[SequenceRow[D]] =
    acquisition // If we are executing Science, don't show any future acquisition rows.
      .filter(_ => isAcquisitionDisplayed)
      .map(List(_))
      .map(futureSteps(SequenceType.Acquisition, currentSequenceType.filter(_ => isExecuting)))
      .orEmpty
