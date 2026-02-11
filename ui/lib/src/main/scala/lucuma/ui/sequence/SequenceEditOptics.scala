// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.syntax.effect.*
import japgolly.scalajs.react.*
import lucuma.core.model.sequence.Atom
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
  )(rowEdit: Step.Id => B => IO[Endo[List[Atom[D]]]])(value: Option[B]): Callback =
    (c.table.options.meta, getFutureStep(c), value).tupled
      .foldMap: (meta, futureStep, v) =>
        rowEdit(futureStep.stepId)(v)
          .flatMap: mod =>
            meta.seqTypeMod(futureStep.seqType)(mod).toAsync
          .runAsyncAndForget

  protected def handleRowEdit[A, B](
    c: CellContextType[A]
  )(rowEdit: Step.Id => B => Endo[List[Atom[D]]])(value: Option[B]): Callback =
    (c.table.options.meta, getFutureStep(c), value).tupled
      .foldMap: (meta, futureStep, v) =>
        meta.seqTypeMod(futureStep.seqType)(rowEdit(futureStep.stepId)(v))

  // Follow this template for other fields
  protected val exposureReplace
    : Step.Id => TimeSpan => Endo[List[Atom[D]]] = // TODO Atom, with traverse
    modifyStep:
      combineOptionalsReplace(
        gmosNorth.andThen(gmos.DynamicConfig.GmosNorth.exposure),
        gmosSouth.andThen(gmos.DynamicConfig.GmosSouth.exposure),
        flamingos2.andThen(Flamingos2DynamicConfig.exposure)
      )

  // Also deletes empty atoms.
  protected val deleteRow: Step.Id => Unit => Endo[List[Atom[D]]] =
    stepId =>
      _ =>
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

  protected val cloneRow: Step.Id => Unit => IO[Endo[List[Atom[D]]]] =
    stepId =>
      _ =>
        IO.randomUUID.map: newId =>
          atomsTraversal
            .andThen(Atom.steps)
            .modify: steps =>
              steps.zipWithIndex
                .collectFirst { case (step, idx) if step.id === stepId => (step, idx) }
                .fold(steps): (step, idx) =>
                  val newStep: Step[D] = Step.id.replace(Step.Id.fromUuid(newId))(step)
                  val (before, after)  = steps.toList.splitAt(idx + 1)
                  NonEmptyList.fromListUnsafe(before ++ (newStep :: after))
