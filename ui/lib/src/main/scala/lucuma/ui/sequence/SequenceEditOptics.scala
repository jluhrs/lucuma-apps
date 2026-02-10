// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.syntax.effect.*
import japgolly.scalajs.react.*
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.util.TimeSpan
import lucuma.react.table.*
import lucuma.ui.table.*
import monocle.Optional
import monocle.Prism
import monocle.Traversal

trait SequenceEditOptics[D, T, R <: SequenceRow[D], TM <: SequenceTableMeta[D], CM, TF](
  getStep: T => Option[R]
):
  private val dynamicConfig: Optional[SequenceRow[D], D] = SequenceRow
    .futureStep[D]
    .andThen(SequenceRow.FutureStep.step)
    .andThen(Step.instrumentConfig)

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

  private val gmosNorth: Optional[SequenceRow[D], gmos.DynamicConfig.GmosNorth] =
    dynamicConfig.andThen(gmosNorthDynamicConfig)

  private val gmosSouth: Optional[SequenceRow[D], gmos.DynamicConfig.GmosSouth] =
    dynamicConfig.andThen(gmosSouthDynamicConfig)

  private val flamingos2: Optional[SequenceRow[D], Flamingos2DynamicConfig] =
    dynamicConfig.andThen(flamingos2DyamicConfig)

  private def combineOptionalsReplace[S, A](optionals: Optional[S, A]*)(a: A): S => S =
    Function.chain(optionals.map(_.replace(a)))

  private val sequenceTraversal: Traversal[List[SequenceRow[D]], SequenceRow[D]] =
    Traversal.fromTraverse[List, SequenceRow[D]]

  private def selectRow(stepId: Step.Id): Traversal[List[SequenceRow[D]], SequenceRow[D]] =
    sequenceTraversal.filter(_.id === stepId.asRight)

  private def modifyRow[A](apply: A => Endo[SequenceRow[D]])(stepId: Step.Id)(
    a: A
  ): Endo[List[SequenceRow[D]]] =
    selectRow(stepId).modify(apply(a))

  private type CellContextType[A] =
    CellContext[Expandable[HeaderOrRow[T]], Option[A], TM, ?, TF, ?, ?]

  private def getFutureStep[A](c: CellContextType[A]): Option[SequenceRow.FutureStep[D]] =
    c.row.original.value
      .map(getStep)
      .toOption
      .flatMap:
        _.collect:
          case SequenceRow.futureStep(fs) => fs

  protected def handleRowEditAsync[A, B](
    c: CellContextType[A]
  )(rowEdit: Step.Id => B => IO[Endo[List[SequenceRow[D]]]])(value: Option[B]): Callback =
    (c.table.options.meta, getFutureStep(c), value).tupled
      .foldMap: (meta, futureStep, v) =>
        rowEdit(futureStep.stepId)(v)
          .flatMap: mod =>
            meta.seqTypeMod(futureStep.seqType)(mod).toAsync
          .runAsyncAndForget

  protected def handleRowEdit[A, B](
    c: CellContextType[A]
  )(rowEdit: Step.Id => B => Endo[List[SequenceRow[D]]])(value: Option[B]): Callback =
    (c.table.options.meta, getFutureStep(c), value).tupled
      .foldMap: (meta, futureStep, v) =>
        meta.seqTypeMod(futureStep.seqType)(rowEdit(futureStep.stepId)(v))

  // Follow this template for other fields
  protected val exposureReplace: Step.Id => TimeSpan => Endo[List[SequenceRow[D]]] =
    modifyRow:
      combineOptionalsReplace(
        gmosNorth.andThen(gmos.DynamicConfig.GmosNorth.exposure),
        gmosSouth.andThen(gmos.DynamicConfig.GmosSouth.exposure),
        flamingos2.andThen(Flamingos2DynamicConfig.exposure)
      )

  protected val deleteRow: Step.Id => Unit => Endo[List[SequenceRow[D]]] =
    stepId =>
      _ =>
        rows =>
          rows.zipWithIndex
            .collectFirst:
              case (SequenceRow.futureStep(fs), idx) if fs.stepId === stepId => idx
            .fold(rows): idx =>
              val (before, after) = rows.splitAt(idx)
              before ++ after.tail

  protected val cloneRow: Step.Id => Unit => IO[Endo[List[SequenceRow[D]]]] =
    stepId =>
      _ =>
        IO.randomUUID.map: newId =>
          rows =>
            rows.zipWithIndex
              .collectFirst:
                case (SequenceRow.futureStep(fs), idx) if fs.stepId === stepId => (fs, idx)
              .foldMap: (fs, idx) =>
                val rowToClone      = SequenceRow.FutureStep.step
                  .andThen(Step.id)
                  .replace(Step.Id(newId))(fs)
                val (before, after) = rows.splitAt(idx + 1)
                before ++ (rowToClone :: after)
