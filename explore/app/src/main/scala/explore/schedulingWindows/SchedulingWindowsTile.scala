// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schedulingWindows

import cats.Endo
import cats.MonadThrow
import cats.Order.given
import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import explore.Icons
import explore.common.TimingWindowsQueries
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.Constants.BadTimingWindow
import explore.model.ObsIdSet
import explore.model.ObsIdSetEditInfo
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.ObservationList
import explore.model.enums.TileSizeState
import explore.model.formats.*
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.render.given
import explore.services.OdbObservationApi
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect.Dispatch
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowEnd
import lucuma.core.model.TimingWindowRepeat
import lucuma.core.syntax.display.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.render.*
import lucuma.ui.table.*
import lucuma.ui.utils.Render
import monocle.Iso
import monocle.Lens
import monocle.Traversal
import monocle.function.Index
import monocle.function.Index.*
import org.typelevel.log4cats.Logger

import java.time.Duration
import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime

sealed abstract class SchedulingWindowsTile(
  val obsEditInfo:   ObsIdSetEditInfo,
  val timingWindows: View[List[TimingWindow]],
  isReadOnly:        Boolean,
  fullSize:          Boolean
) extends Tile[SchedulingWindowsTile](
      ObsTabTileIds.TimingWindowsId.id,
      if (timingWindows.get.isEmpty) "Scheduling Windows"
      else s"Scheduling Windows (${timingWindows.get.length})",
      canMaximize = !fullSize,
      canMinimize = !fullSize
    )(SchedulingWindowsTile):
  val readonly = isReadOnly || obsEditInfo.allAreCompleted

object SchedulingWindowsTile
    extends TileComponent[SchedulingWindowsTile]({ (props, tileSize) =>
      // Helper lens for converting between Timestamp and Instant
      val timestampToInstant: Lens[Timestamp, Instant] =
        Lens[Timestamp, Instant](_.toInstant): instant =>
          _ => Timestamp.unsafeFromInstantTruncated(instant)

      val msg: Option[String] =
        if (props.readonly)
          none
        else if (props.obsEditInfo.allAreCompleted)
          if (props.obsEditInfo.editing.length > 1)
            "All of the current observations are completed. Scheduling windows are readonly.".some
          else "The current observation has been completed. Scheduling windows are readonly.".some
        else if (props.obsEditInfo.completed.isDefined)
          "Some of the current observations are completed. Only uncompleted observations will be modified.".some
        else
          none

      val ColDef = ColumnDef[(TimingWindow, Int)].WithTableMeta[TableMeta]

      // Update function depends on current observation selection, so we cannot memoize it in the column definition
      case class TableMeta(updateWindows: Endo[List[TimingWindow]] => Callback)

      given Render[TimingWindowInclusion] = Render.by: swt =>
        <.span(swt match
          case TimingWindowInclusion.Include => ExploreStyles.TimingWindowInclude
          case TimingWindowInclusion.Exclude => ExploreStyles.TimingWindowExclude)(swt.shortName)

      given Render[TimingWindow] = Render.by:
        case TimingWindow(inclusion, start, Some(TimingWindowEnd.At(endAt)))           =>
          React.Fragment(
            inclusion.renderVdom,
            " ",
            <.b(start.formatUtcWithZone),
            " through ",
            <.b(endAt.formatUtcWithZone)
          )
        case TimingWindow(inclusion, start, Some(after @ TimingWindowEnd.After(_, _))) =>
          React.Fragment(
            inclusion.renderVdom,
            " ",
            <.b(start.formatUtcWithZone),
            " ",
            after.renderVdom
          )
        case TimingWindow(inclusion, start, None)                                      =>
          React.Fragment(inclusion.renderVdom, " ", <.b(start.formatUtcWithZone), " forever")

      val DeleteColWidth: Int   = 20
      val WindowColId: ColumnId = ColumnId("TimingWindow")
      val DeleteColId: ColumnId = ColumnId("Delete")

      for {
        resize <- useResizeDetector
        // cols
        cols   <- useMemo(()): _ =>
                    List(
                      ColDef(WindowColId, _._1, size = 400.toPx).withCell: cell =>
                        <.span(
                          cell.value.renderVdom,
                          <.span(Icons.ErrorIcon)
                            .withTooltip(BadTimingWindow)
                            .unless(cell.value.isValid)
                        ),
                      ColDef(DeleteColId, _._2, size = DeleteColWidth.toPx).withCell: cell =>
                        Button(
                          text = true,
                          onClickE = e =>
                            e.stopPropagationCB >>
                              cell.table.options.meta
                                .map:
                                  _.updateWindows(sws =>
                                    sws.take(cell.value) ++ sws.drop(cell.value + 1)
                                  )
                                .orEmpty
                        ).compact.small(Icons.Trash)
                    )
        // rows
        rows   <- useMemo(props.timingWindows.get):
                    _.zipWithIndex.sorted
        table  <- useReactTable:
                    TableOptions(
                      cols,
                      rows,
                      enableRowSelection = true,
                      getRowId = (row, _, _) => RowId(row._2.toString),
                      state = PartialTableState(
                        columnSizing = ColumnSizing(
                          WindowColId -> resize.width
                            .map(w => (w - DeleteColWidth).toPx)
                            .getOrElse(400.toPx)
                        ),
                        columnVisibility = ColumnVisibility(
                          DeleteColId -> Visibility.fromVisible(!props.readonly)
                        )
                      ),
                      meta = TableMeta(props.timingWindows.mod)
                    )
      } yield
        val title =
          <.span(
            if (props.readonly || tileSize.isMinimized) EmptyVdom
            else
              Button(
                severity = Button.Severity.Success,
                icon = Icons.New,
                label = "Add",
                onClick = props.timingWindows.mod(
                  _ :+ TimingWindow(
                    inclusion = TimingWindowInclusion.Include,
                    start = Timestamp.unsafeFromInstantTruncated(Instant.now),
                    end = none
                  )
                ) >> table.setRowSelection(
                  RowSelection(RowId(props.timingWindows.get.size.toString) -> true)
                )
              ).tiny.compact
          )

        val pos: Option[Int] = table.getSelectedRowModel().rows.headOption.map(_.original._2)

        val selectedTW: Option[View[TimingWindow]] =
          pos
            .filterNot(_ => props.readonly)
            .flatMap(p =>
              props.timingWindows
                .zoom(Index.index[List[TimingWindow], Int, TimingWindow](p))
                .asView
            )

        val body =
          React.Fragment(
            msg.map(msg => <.div(msg, ExploreStyles.SharedEditWarning)),
            <.div(ExploreStyles.TimingWindowsBody)(
              <.div.withRef(resize.ref)(ExploreStyles.TimingWindowsTable)(
                PrimeTable(
                  table,
                  striped = true,
                  compact = Compact.Very,
                  headerMod = ExploreStyles.TimingWindowsHeader,
                  tableMod =
                    ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable |+| ExploreStyles.TimingWindowsTable,
                  rowMod = rowTagMod: row =>
                    TagMod(
                      ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                      ^.onClick --> (table.toggleAllRowsSelected(false) >> row.toggleSelected())
                    ),
                  cellMod = cellTagMod:
                    _.column.id match
                      case DeleteColId => ^.textAlign.right
                      case _           => TagMod.empty
                  ,
                  // If cmd is pressed add to the selection
                  emptyMessage =
                    <.div(ExploreStyles.ExploreTableEmpty, "No scheduling windows defined")
                )
              ),
              selectedTW.map: tw =>
                val selectedInclusion: View[TimingWindowInclusion]   = tw.zoom(TimingWindow.inclusion)
                val selectedStart: View[Timestamp]                   = tw.zoom(TimingWindow.start)
                val selectedEnd: View[Option[TimingWindowEnd]]       = tw.zoom(TimingWindow.end)
                val selectedEndAt: ViewOpt[Timestamp]                =
                  selectedEnd
                    .zoom(Iso.id[Option[TimingWindowEnd]].some)
                    .zoom(TimingWindowEnd.at)
                    .zoom(TimingWindowEnd.At.instant)
                val selectedEndAfter: ViewOpt[TimingWindowEnd.After] =
                  selectedEnd
                    .zoom(Iso.id[Option[TimingWindowEnd]].some)
                    .zoom(TimingWindowEnd.after)

                def renderInclusionRadio(twt: TimingWindowInclusion, id: String): VdomNode =
                  <.span(
                    RadioButton(
                      twt,
                      id = id,
                      checked = selectedInclusion.get === twt,
                      onChange = (v, checked) => selectedInclusion.set(v).when_(checked)
                    ),
                    <.label(^.htmlFor := id, twt.renderVdom)
                  )

                <.div(ExploreStyles.TimingWindowEditor)(
                  <.div(ExploreStyles.TimingWindowInclusionEditor)(
                    renderInclusionRadio(TimingWindowInclusion.Include, "include-option"),
                    renderInclusionRadio(TimingWindowInclusion.Exclude, "exclude-option")
                  ),
                  <.div(ExploreStyles.TimingWindowFromEditor)(
                    <.label(^.htmlFor := "from-time-picker", " from"),
                    DatePicker24HTime(
                      "from-time-picker".refined,
                      selectedStart.zoom(timestampToInstant),
                      props.readonly,
                      maxDate = selectedEnd.get
                        .flatMap(TimingWindowEnd.at.getOption)
                        .map(_.instant.toInstant)
                    ),
                    <.span(" UTC "),
                    <.span(Icons.ErrorIcon)
                      .withTooltip("Check start date is before the end")
                      .unless(tw.get.isValid)
                  ),
                  <.div(ExploreStyles.TimingWindowEditorBody)(
                    <.div(
                      RadioButton(
                        "forever",
                        id = "forever-option",
                        checked = selectedEnd.get.isEmpty,
                        onChange = (_, checked) => selectedEnd.set(none).when_(checked)
                      ),
                      <.label("Forever", ^.htmlFor := "forever-option")
                    ),
                    <.div(
                      ExploreStyles.TimingWindowThroughEditor,
                      RadioButton(
                        "through",
                        id = "through-option",
                        checked = selectedEndAt.get.isDefined,
                        onChange = (_, checked) =>
                          selectedEnd
                            .set(
                              TimingWindowEnd
                                .At(
                                  Timestamp.unsafeFromInstantTruncated(
                                    ZonedDateTime
                                      .ofInstant(selectedStart.get.toInstant, ZoneOffset.UTC)
                                      .plusHours(1)
                                      .toInstant
                                  )
                                )
                                .some
                            )
                            .when_(checked)
                      ),
                      <.label("Through ", ^.htmlFor := "through-option"),
                      selectedEndAt.mapValue(endAt =>
                        React.Fragment(
                          DatePicker24HTime(
                            "through-time-picker".refined,
                            endAt.zoom(timestampToInstant),
                            props.readonly,
                            minDate = selectedStart.get.toInstant.some
                          ),
                          <.span(" UTC "),
                          if (tw.get.isValid) EmptyVdom
                          else
                            <.span(Icons.ErrorIcon)
                              .withTooltip("Check start date is before the end")
                        )
                      )
                    ),
                    <.div(ExploreStyles.TimingWindowEndAfter)(
                      RadioButton(
                        "for",
                        id = "for",
                        checked = selectedEndAfter.get.isDefined,
                        onChange = (_, checked) =>
                          selectedEnd
                            .set(
                              TimingWindowEnd
                                .After(
                                  duration = TimeSpan.unsafeFromDuration(Duration.ofDays(2)),
                                  repeat = none
                                )
                                .some
                            )
                            .when_(checked)
                      ),
                      <.label("For", ^.htmlFor := "for"),
                      selectedEndAfter.mapValue { endAfter =>
                        React.Fragment(
                          FormInputTextView(
                            id = "for-duration".refined,
                            value = endAfter.zoom(TimingWindowEnd.After.duration),
                            validFormat = durationHM,
                            changeAuditor = hmChangeAuditor
                          ),
                          " hours"
                        )
                      }
                    ),
                    selectedEndAfter.mapValue { endAfter =>
                      val selectedRepeat: View[Option[TimingWindowRepeat]] =
                        endAfter.zoom(TimingWindowEnd.After.repeat)
                      val selectedRepeatOpt: ViewOpt[TimingWindowRepeat]   =
                        selectedRepeat.zoom(Iso.id[Option[TimingWindowRepeat]].some)
                      val selectedRepeatPeriod: ViewOpt[TimeSpan]          =
                        selectedRepeatOpt.zoom(TimingWindowRepeat.period)

                      <.div(
                        <.div(ExploreStyles.TimingWindowRepeatEditor)(
                          <.div(LucumaPrimeStyles.CheckboxWithLabel)(
                            Checkbox(
                              id = "repeat-with-period",
                              checked = selectedRepeat.get.isDefined,
                              onChange = checked =>
                                selectedRepeat
                                  .set(
                                    TimingWindowRepeat(
                                      period = TimeSpan
                                        .unsafeFromDuration(
                                          endAfter.get.duration.toDuration.plusHours(12)
                                        ),
                                      times = none
                                    ).some
                                  )
                                  .when_(checked) >>
                                  selectedRepeat.set(none).unless_(checked)
                            ),
                            <.label("Repeat with a period of", ^.htmlFor := "repeat-with-period")
                          ),
                          FormInputTextView(
                            id = "repat-period".refined,
                            value = selectedRepeatPeriod,
                            validFormat = durationHMS,
                            changeAuditor = hmsChangeAuditor,
                            disabled = selectedRepeat.get.isEmpty
                          ),
                          " hours"
                        ),
                        selectedRepeatOpt.mapValue { repeat =>
                          val selectedRepeatTimes: View[Option[PosInt]] =
                            repeat.zoom(TimingWindowRepeat.times)

                          <.div(ExploreStyles.TimingWindowRepeatEditorAlternatives)(
                            <.div(
                              RadioButton(
                                "repeat-forever",
                                id = "repeat-forever-option",
                                checked = selectedRepeatTimes.get.isEmpty,
                                onChange =
                                  (_, checked) => selectedRepeatTimes.set(none).when_(checked)
                              ),
                              <.label("Forever", ^.htmlFor := "repeat-forever-option")
                            ),
                            <.div(ExploreStyles.TimingWindowRepeatEditorNTimes)(
                              RadioButton(
                                "repeat-n-times",
                                id = "repeat-n-times",
                                checked = selectedRepeatTimes.get.isDefined,
                                onChange = (_, checked) =>
                                  selectedRepeatTimes.set(1.refined[Positive].some).when_(checked)
                              ),
                              FormInputTextView(
                                id = "repeat-n-times-value".refined,
                                value = selectedRepeatTimes.zoom(Iso.id[Option[PosInt]].some),
                                validFormat = InputValidSplitEpi.posInt,
                                changeAuditor = ChangeAuditor.posInt,
                                disabled = selectedRepeatTimes.get.isEmpty
                              ),
                              <.label("times", ^.htmlFor := "repeat-n-times-value")
                            )
                          )
                        }
                      )
                    }
                  )
                )
            )
          )

        TileContents(title, body)
    })

final case class ObservationSchedulingWindowsTile[F[
  _
]: {OdbObservationApi, MonadThrow, Dispatch, Logger, ToastCtx}](
  observation: UndoSetter[Observation],
  isReadonly:  Boolean,
  fullSize:    Boolean
) extends SchedulingWindowsTile(
      ObsIdSetEditInfo.of(observation.get),
      TimingWindowsQueries.viewWithRemoteMod(
        ObsIdSet.one(observation.get.id),
        observation.undoableView[List[TimingWindow]](Observation.timingWindows)
      ),
      isReadonly,
      fullSize
    )

final case class ObsIdSetSchedulingWindowsTile[F[
  _
]: {OdbObservationApi, MonadThrow, Dispatch, Logger, ToastCtx}](
  override val obsEditInfo: ObsIdSetEditInfo,
  observations:             UndoSetter[ObservationList],
  isReadonly:               Boolean,
  fullSize:                 Boolean
) extends SchedulingWindowsTile(
      obsEditInfo, {
        // We will only edit the incomplete observations, but we noed something to pass
        // to the timing windows panel. However, if all are complete, it will be readonly
        val idsToEdit: ObsIdSet = obsEditInfo.unCompleted.getOrElse(obsEditInfo.editing)

        val obsTraversal: Traversal[ObservationList, Observation] =
          Iso
            .id[ObservationList]
            .filterIndex((id: Observation.Id) => idsToEdit.contains(id))

        val twTraversal = obsTraversal.andThen(Observation.timingWindows)

        TimingWindowsQueries.viewWithRemoteMod(
          idsToEdit,
          observations
            .undoableView[List[TimingWindow]](
              twTraversal.getAll.andThen(_.head),
              twTraversal.modify
            )
        )
      },
      isReadonly,
      fullSize
    )
