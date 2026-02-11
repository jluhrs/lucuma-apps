// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.syntax.effect.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.util.TimeSpan
import lucuma.react.table.*
import lucuma.ui.table.*
import monocle.Iso
import monocle.Optional
import monocle.Prism
import monocle.Traversal

trait SequenceEditOptics[D, T, R <: SequenceRow[D], TM <: SequenceTableMeta[D], CM, TF](
  getStepFromRow: T => Option[R]
):
  private val gmosDynamicConfig: Prism[D, gmos.DynamicConfig] =
    Prism[D, gmos.DynamicConfig] {
      case g: gmos.DynamicConfig => Some(g)
      case _                     => None
    }(_.asInstanceOf[D])

  private val gmosNorthDynamicConfig: Prism[D, gmos.DynamicConfig.GmosNorth] =
    gmosDynamicConfig.andThen(gmos.DynamicConfig.gmosNorth)

  private val gmosSouthDynamicConfig: Prism[D, gmos.DynamicConfig.GmosSouth] =
    gmosDynamicConfig.andThen(gmos.DynamicConfig.gmosSouth)

  private val flamingos2DyamicConfig: Prism[D, Flamingos2DynamicConfig] =
    Prism[D, Flamingos2DynamicConfig] {
      case f2: Flamingos2DynamicConfig => Some(f2)
      case _                           => None
    }(_.asInstanceOf[D])

  private val gmosNorth: Optional[Step[D], gmos.DynamicConfig.GmosNorth] =
    Step.instrumentConfig.andThen(gmosNorthDynamicConfig)

  private val gmosSouth: Optional[Step[D], gmos.DynamicConfig.GmosSouth] =
    Step.instrumentConfig.andThen(gmosSouthDynamicConfig)

  private val flamingos2: Optional[Step[D], Flamingos2DynamicConfig] =
    Step.instrumentConfig.andThen(flamingos2DyamicConfig)

  private def combineOptionalsReplace[S, A](optionals: Optional[S, A]*)(a: A): S => S =
    Function.chain(optionals.map(_.replace(a)))

  private val atomsTraversal: Traversal[List[Atom[D]], Atom[D]] =
    Traversal.fromTraverse[List, Atom[D]]

  private val stepsTraversal: Traversal[NonEmptyList[Step[D]], Step[D]] =
    Traversal.fromTraverse[NonEmptyList, Step[D]]

  private def selectStep(stepId: Step.Id): Traversal[NonEmptyList[Step[D]], Step[D]] =
    stepsTraversal.filter(_.id === stepId)

  private def modifyStep[A](apply: A => Endo[Step[D]])(stepId: Step.Id)(a: A): Endo[List[Atom[D]]] =
    atomsTraversal.andThen(Atom.steps).andThen(selectStep(stepId)).modify(apply(a))

  protected type CellContextType[A] =
    CellContext[Expandable[HeaderOrRow[T]], A, TM, ?, TF, ?, ?]

  extension (row: Expandable[HeaderOrRow[T]])
    protected def getStep: Option[R] =
      row.value.toOption.flatMap(row => getStepFromRow(row))

  extension [A](cellContext: CellContextType[A])
    protected def getStep: Option[R] =
      cellContext.row.original.getStep

    protected def getFutureStep: Option[SequenceRow.FutureStep[D]] =
      cellContext.getStep.collect:
        case SequenceRow.futureStep(fs) => fs

  protected def handleSeqTypeRowEditAsync[A](
    c:       CellContextType[A],
    seqType: SequenceType
  )(rowEdit: IO[Endo[List[Atom[D]]]]): Callback =
    c.table.options.meta.foldMap: meta =>
      rowEdit
        .flatMap: mod =>
          meta.seqTypeMod(seqType)(mod).toAsync
        .runAsyncAndForget

  protected def handleAllSeqTypesRowEditAsync[A](c: CellContextType[A])(
    rowEdit: IO[Endo[List[Atom[D]]]]
  ): Callback =
    handleSeqTypeRowEditAsync(c, SequenceType.Acquisition)(rowEdit) >>
      handleSeqTypeRowEditAsync(c, SequenceType.Science)(rowEdit)

  protected def handleSeqTypeRowEdit[A](c: CellContextType[A], seqType: SequenceType)(
    rowEdit: Endo[List[Atom[D]]]
  ): Callback =
    c.table.options.meta.foldMap(_.seqTypeMod(seqType)(rowEdit))

  protected def handleRowEdit[A](c: CellContextType[A])(rowEdit: Endo[List[Atom[D]]]): Callback =
    c.getFutureStep.foldMap: futureStep =>
      handleSeqTypeRowEdit(c, futureStep.seqType)(rowEdit)

  protected def handleRowValueEdit[A, B](
    c: CellContextType[A]
  )(rowEdit: Step.Id => B => Endo[List[Atom[D]]])(value: Option[B]): Callback =
    (c.getFutureStep, value).tupled.foldMap: (futureStep, v) =>
      handleSeqTypeRowEdit(c, futureStep.seqType)(rowEdit(futureStep.stepId)(v))

  // Follow this template for other fields
  protected val exposureReplace: Step.Id => TimeSpan => Endo[List[Atom[D]]] =
    modifyStep:
      combineOptionalsReplace(
        gmosNorth.andThen(gmos.DynamicConfig.GmosNorth.exposure),
        gmosSouth.andThen(gmos.DynamicConfig.GmosSouth.exposure),
        flamingos2.andThen(Flamingos2DynamicConfig.exposure)
      )

  // Also deletes empty atoms.
  protected val deleteRow: Step.Id => Endo[List[Atom[D]]] =
    stepId =>
      _.map: atom =>
        val steps: List[Step[D]]                    = atom.steps.toList
        val newSteps: Option[NonEmptyList[Step[D]]] =
          NonEmptyList.fromList:
            steps.zipWithIndex
              .collectFirst { case (step, idx) if step.id === stepId => idx }
              .fold(steps): idx =>
                val (before, after) = steps.splitAt(idx)
                before ++ after.tail
        newSteps.map(ns => Atom.steps.replace(ns)(atom))
      .flattenOption

  protected def cloneRow(row: SequenceRow[D]): IO[Endo[List[Atom[D]]]] =
    IO.randomUUID
      .map(Step.Id.fromUuid(_))
      .map: newId =>
        row match
          case SequenceRow.futureStep(fs)   =>
            atomsTraversal
              .andThen(Atom.steps)
              .modify: steps =>
                steps.zipWithIndex
                  .collectFirst { case (step, idx) if step.id === fs.stepId => (step, idx) }
                  .fold(steps): (step, idx) =>
                    val newStep: Step[D] = Step.id.replace(newId)(step)
                    val (before, after)  = steps.toList.splitAt(idx + 1)
                    NonEmptyList.fromListUnsafe(before ++ (newStep :: after))
          case SequenceRow.executedStep(es) =>
            val newStep: Step[D] = Step(
              newId,
              es.stepRecord.instrumentConfig,
              es.stepRecord.stepConfig,
              es.stepRecord.telescopeConfig,
              // TODO IS THIS WHAT WE ACTUALLY WANT?? // es.interval.foldMap(_.timeSpan),
              StepEstimate.fromMax(List.empty, List.empty),
              es.stepRecord.observeClass,
              Breakpoint.Disabled
            )
            Iso
              .id[List[Atom[D]]]
              .modify:
                // Sequence is complete, we must create a new atom to hold the cloned step?
                case Nil          => Nil
                case head :: tail =>
                  val newHead: Atom[D] =
                    if head.observeClass === newStep.observeClass || // Calibration steps go to Science sequence
                      (head.observeClass === ObserveClass.Science && newStep.observeClass.isCalibration)
                    then Atom.steps[D].modify(newStep :: _)(head)
                    else head
                  newHead :: tail
          case _                            => identity
