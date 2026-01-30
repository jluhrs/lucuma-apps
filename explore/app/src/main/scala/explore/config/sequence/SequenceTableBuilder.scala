// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Endo
import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.react.SizePx
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*
import lucuma.core.model.sequence.Step

import scala.scalajs.LinkingInfo
import monocle.Lens
import monocle.Optional
import monocle.Focus

private type SequenceColumnsType[D] =
  SequenceColumns[D, SequenceIndexedRow[D], SequenceRow[D], Nothing, Nothing, Nothing]
private type ColumnType[D]          =
  ColumnDef[Expandable[HeaderOrRow[SequenceIndexedRow[D]]], ?, Nothing, Nothing, Nothing, Any, Any]

private trait SequenceTableBuilder[S, D: Eq](instrument: Instrument) extends SequenceRowBuilder[D]:
  private type Props = SequenceTable[S, D]

  private case class TableMeta[D](
    isEditing: IsEditing = IsEditing.False,
    modRow:    Step.Id => Endo[SequenceRow[D]] => Callback
  ) extends SequenceTableMeta[D]

  private lazy val ColDef = ColumnDef[SequenceTableRowType].WithTableMeta[TableMeta[D]]

  private val HeaderColumnId: ColumnId   = ColumnId("header")
  private val ExtraRowColumnId: ColumnId = ColumnId("extraRow")

  private lazy val ColumnSizes: Map[ColumnId, ColumnSize] = Map(
    HeaderColumnId   -> FixedSize(0.toPx),
    ExtraRowColumnId -> FixedSize(0.toPx)
  ) ++ SequenceColumns.BaseColumnSizes(instrument)

  private lazy val columns: Reusable[List[ColDef.Type]] =
    Reusable.always:
      List(
        SequenceColumns
          .headerCell(HeaderColumnId, ColDef)
          .withColumnSize(ColumnSizes(HeaderColumnId)),
        ColDef(
          ExtraRowColumnId,
          header = "",
          cell = _.row.original.value.toOption
            .map(_.step)
            .collect:
              case step @ SequenceRow.Executed.ExecutedStep(_, _) =>
                renderVisitExtraRow(step, showOngoingLabel = true)
        ).withColumnSize(ColumnSizes(ExtraRowColumnId))
      ) ++ SequenceColumns(ColDef, _.step.some, _.index.some)(instrument)

  private lazy val DynTableDef = DynTable(
    ColumnSizes,
    SequenceColumns.BaseColumnPriorities(instrument),
    DynTable.ColState(
      resized = ColumnSizing(),
      visibility = ColumnVisibility()
    )
  )

  // TODO These should actually just be SequenceRow.FutureStep
  private case class SequenceRows(acquisition: List[SequenceRow[D]], science: List[SequenceRow[D]])
      derives Eq
  private object SequenceRows:
    val acquisition: Lens[SequenceRows, List[SequenceRow[D]]] = Focus[SequenceRows](_.acquisition)
    val science: Lens[SequenceRows, List[SequenceRow[D]]]     = Focus[SequenceRows](_.science)

    given Reusability[SequenceRows] = Reusability.byEq

  // We maintain two copies of the rows. When in View mode, they both contain the same set of rows.
  // When in Edit mode, the `view` copy continues to be updated with external changes,
  // while the `edit` copy remains static to preserve the user's edits.
  private case class SequenceCopies(view: SequenceRows, edit: SequenceRows)
  private object SequenceCopies:
    val view: Lens[SequenceCopies, SequenceRows] = Focus[SequenceCopies](_.view)
    val edit: Lens[SequenceCopies, SequenceRows] = Focus[SequenceCopies](_.edit)

  protected[sequence] val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx            <- useContext(AppContext.ctx)
        visitsData     <- useMemo(props.visits):
                            visitsSequences(_, none)
        sequenceCopies <- useStateView:
                            SequenceCopies(
                              view = SequenceRows(props.acquisitionRows,
                                                  props.scienceRows
                              ), // This shouldn't be in a View
                              edit = SequenceRows(props.acquisitionRows, props.scienceRows)
                            )
        _              <- useEffectWithDeps(props.i): _ =>
                            // TODO This is a temporary mechanism for demo purposes
                            sequenceCopies.mod(sc => sc.copy(view = sc.edit))
        _              <- useEffectWithDeps(props.isEditing): _ =>
                            sequenceCopies.mod(sc => sc.copy(edit = sc.view))
        // Update our copy of the sequence when it changes externally.
        _              <- useEffectWithDeps(props.acquisitionRows, props.scienceRows): (acquisiton, science) =>
                            sequenceCopies.mod:
                              val newRows: SequenceRows = SequenceRows(acquisiton, science)
                              SequenceCopies.view.replace(newRows) >>>
                                (if props.isEditing then SequenceCopies.edit.replace(newRows) else identity)
        effectiveRows   = if props.isEditing then sequenceCopies.get.edit else sequenceCopies.get.view
        rows           <-
          useMemo(
            (visitsData, effectiveRows, props.currentVisitId)
          ): (visitsData, rows, currentVisitId) =>
            val (visitRows, nextScienceIndex): (List[VisitData], StepIndex) = visitsData.value
            stitchSequence(
              visitRows,
              currentVisitId,
              nextScienceIndex,
              rows.acquisition,
              rows.science
            )
        resize         <- useResizeDetector
        dynTable       <- useDynTable(DynTableDef, SizePx(resize.width.orEmpty))
        table          <-
          useReactTable:
            // TODO For demo purposes, this should be a map and avoid unsafe gets.
            def modSequenceRow(rows: View[List[SequenceRow[D]]])(stepId: Step.Id)(
              mod: Endo[SequenceRow[D]]
            ): Callback =
              rows
                .zoom(
                  Optional[List[SequenceRow[D]], SequenceRow[D]] { rs =>
                    rs.find(_.id === stepId.asRight)
                  } { updatedRow => rs =>
                    rs.map:
                      case row if row.id === stepId.asRight => updatedRow
                      case row                              => row
                  }
                )
                .mod(mod)

            def modRow(stepId: Step.Id)(mod: Endo[SequenceRow[D]]): Callback =
              val editView: View[SequenceRows] = sequenceCopies.zoom(SequenceCopies.edit)
              modSequenceRow(editView.zoom(SequenceRows.acquisition))(stepId)(mod) >>
                modSequenceRow(editView.zoom(SequenceRows.science))(stepId)(mod)

            TableOptions(
              columns.map(dynTable.setInitialColWidths),
              rows,
              enableSorting = false,
              enableColumnResizing = true,
              enableExpanding = true,
              getRowId = (row, _, _) => getRowId(row),
              getSubRows = (row, _) => row.subRows,
              columnResizeMode = ColumnResizeMode.OnChange,
              initialState = TableState(
                expanded = CurrentExpandedState
              ),
              state = PartialTableState(
                columnSizing = dynTable.columnSizing,
                columnVisibility = dynTable.columnVisibility
              ),
              onColumnSizingChange = dynTable.onColumnSizingChangeHandler,
              meta = TableMeta(props.isEditing, modRow)
            )
      yield
        val extraRowMod: TagMod =
          TagMod(
            SequenceStyles.ExtraRowShown,
            resize.width
              .map: w =>
                ^.width := s"${w}px"
              .whenDefined
          )

        PrimeAutoHeightVirtualizedTable(
          table,
          estimateSize = index =>
            table.getRowModel().rows.get(index).map(_.original.value) match
              case Some(Right(SequenceIndexedRow(SequenceRow.Executed.ExecutedStep(_, _), _))) =>
                SequenceRowHeight.WithExtra
              case _                                                                           =>
                SequenceRowHeight.Regular,
          overscan = 8,
          containerRef = resize.ref,
          compact = Compact.Very,
          hoverableRows = true,
          celled = true,
          tableMod = SequenceStyles.SequenceTable,
          headerCellMod = _.column.id match
            case id if id == HeaderColumnId   => SequenceStyles.HiddenColTableHeader
            case id if id == ExtraRowColumnId => SequenceStyles.HiddenColTableHeader
            case _                            => TagMod.empty,
          rowMod = rowTagMod:
            _.original.value.fold(
              _ => ExploreStyles.SequenceRowHeader,
              stepRow =>
                val step: SequenceRow[D] = stepRow.step
                TagMod(
                  step match
                    case SequenceRow.Executed.ExecutedStep(step, _)                    =>
                      SequenceStyles.RowHasExtra |+|
                        ExploreStyles.SequenceRowDone.unless_(
                          step.executionState == StepExecutionState.Ongoing
                        )
                    case SequenceRow.FutureStep(_, _, firstOf, _) if firstOf.isDefined =>
                      ExploreStyles.SequenceRowFirstInAtom
                    case _                                                             => TagMod.empty,
                  if (LinkingInfo.developmentMode)
                    step.id.toOption.map(^.title := _.toString).whenDefined
                  else TagMod.empty
                )
            )
          ,
          cellMod = cellTagMod: cell =>
            cell.row.original.value match
              case Left(_)        => // Header
                cell.column.id match
                  case id if id == HeaderColumnId => TagMod(^.colSpan := columns.length)
                  case _                          => ^.display.none
              case Right(stepRow) =>
                cell.column.id match
                  case id if id == ExtraRowColumnId =>
                    stepRow.step match // Extra row is shown in a selected row or in an executed step row.
                      case SequenceRow.Executed.ExecutedStep(_, _) => extraRowMod
                      case _                                       => TagMod.empty
                  case _                            =>
                    TagMod.empty
        )
