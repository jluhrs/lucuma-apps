// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.Focused
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.TargetList
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Display
import lucuma.react.common.Css
import lucuma.react.primereact.Button
import lucuma.react.primereact.PrimeStyles
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Iso
import org.scalajs.dom.File as DOMFile

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

final case class TargetSummaryTile(
  userId:             Option[User.Id],
  programId:          Program.Id,
  targets:            View[TargetList],
  targetObservations: Map[Target.Id, SortedSet[Observation.Id]],
  selectObservation:  (Observation.Id, Target.Id) => Callback,
  selectedTargetIds:  View[List[Target.Id]],
  focusedTargetId:    Option[Target.Id],
  focusTargetId:      Option[Target.Id] => Callback,
  readonly:           Boolean,
  backButton:         VdomNode
) extends Tile[TargetSummaryTile](
      ObsTabTileIds.TargetSummaryId.id,
      s"Target Summary (${targets.get.size})",
      renderBackButton = backButton.some,
      canMinimize = false,
      canMaximize = false
    )(TargetSummaryTile)

object TargetSummaryTile
    extends TileComponent[TargetSummaryTile]({ (props, _) =>
      val IdColumnId: ColumnId           = ColumnId("id")
      val CountColumnId: ColumnId        = ColumnId("count")
      val ObservationsColumnId: ColumnId = ColumnId("observations")

      val ColNames: TreeSeqMap[ColumnId, String] =
        TreeSeqMap(IdColumnId -> "Id") ++
          TargetColumns.AllColNames ++
          TreeSeqMap(
            CountColumnId        -> "Count",
            ObservationsColumnId -> "Observations"
          )

      given Display[TargetDisposition] = Display.byShortName:
        case TargetDisposition.Science     => "Science"
        case TargetDisposition.Calibration => "Calibration"
        case TargetDisposition.BlindOffset => "Blind Offset"

      val ColDef = ColumnDef[TargetWithId]

      val ColumnClasses: Map[ColumnId, Css] = Map(
        IdColumnId                 -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
        TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
        TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
      )

      val ScrollOptions =
        rawVirtual.mod
          .ScrollToOptions()
          .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
          .setAlign(rawVirtual.mod.ScrollAlignment.center)

      def column[V](id: ColumnId, accessor: TargetWithId => V) =
        ColDef(id, row => accessor(row), ColNames(id))

      def columns(
        ctx:                AppContext[IO],
        programId:          Program.Id,
        targetObservations: Map[Target.Id, SortedSet[Observation.Id]],
        focusTargetId:      Option[Target.Id] => Callback,
        selectObservation:  (Observation.Id, Target.Id) => Callback
      ) =
        List(
          ColDef(
            IdColumnId,
            _.id,
            "id",
            cell =>
              <.a(
                ^.href := ctx.pageUrl(
                  (AppTab.Targets, programId, Focused.target(cell.value)).some
                ),
                ^.onClick ==> (e =>
                  e.preventDefaultCB >> e.stopPropagationCB >>
                    focusTargetId(cell.value.some)
                )
              )(
                cell.value.toString
              )
          ).sortable
        ) ++
          TargetColumns.Builder.ForProgram(ColDef, _.target.regionOrBaseCoords).AllColumns ++
          List(
            column(
              CountColumnId,
              x => targetObservations.get(x.id).map(_.size).orEmpty
            ) // TODO Right align
              .withCell(_.value.toString),
            column(
              ObservationsColumnId,
              x => (x.id, targetObservations.get(x.id).orEmpty.toList)
            )
              .withCell(cell =>
                val (tid, obsIds) = cell.value
                <.span(
                  obsIds
                    .map(obsId =>
                      <.a(
                        ^.href := ctx.pageUrl(
                          (AppTab.Targets, programId, Focused.singleObs(obsId, tid.some)).some
                        ),
                        ^.onClick ==> (e =>
                          e.preventDefaultCB >> e.stopPropagationCB >>
                            selectObservation(obsId, cell.row.original.id)
                        ),
                        obsId.show
                      )
                    )
                    .mkReactFragment(", ")
                )
              )
              .withEnableSorting(false)
          )

      val targetIds2RowSelection: Iso[List[Target.Id], RowSelection] =
        Iso[List[Target.Id], RowSelection](targetIds =>
          RowSelection:
            targetIds.map(targetId => RowId(targetId.toString) -> true).toMap
        )(selection =>
          selection.value
            .filter(_._2)
            .keys
            .toList
            .map(rowId => Target.Id.parse(rowId.value))
            .flattenOption
        )

      for
        ctx              <- useContext(AppContext.ctx)
        filesToImport    <- useStateView(List.empty[DOMFile])
        disposition      <- useStateView(TargetDisposition.Science)
        columnVisibility <- useStateView(TargetColumns.DefaultVisibility)
        cols             <- useMemo(()): _ =>
                              columns(
                                ctx,
                                props.programId,
                                props.targetObservations,
                                props.focusTargetId,
                                props.selectObservation
                              )
        rows             <- useMemo(props.targets.get, disposition.get): (targets, disposition) =>
                              targets.toList
                                .filter((_, twid) => twid.disposition === disposition)
                                .map(_._2)
        table            <- useReactTableWithStateStore:
                              import ctx.given

                              val rowSelection: View[RowSelection] =
                                props.selectedTargetIds.as(targetIds2RowSelection)

                              TableOptionsWithStateStore(
                                TableOptions(
                                  cols,
                                  rows,
                                  getRowId = (row, _, _) => RowId(row.id.toString),
                                  enableSorting = true,
                                  enableColumnResizing = true,
                                  columnResizeMode = ColumnResizeMode.OnChange,
                                  enableMultiRowSelection = true,
                                  state = PartialTableState(
                                    columnVisibility = columnVisibility.get,
                                    rowSelection = rowSelection.get
                                  ),
                                  onColumnVisibilityChange = stateInViewHandler(columnVisibility.mod),
                                  onRowSelectionChange = stateInViewHandler(
                                    rowSelection
                                      .withOnMod: rs =>
                                        // We'll only unfocus if something is selected. Otherwise this is
                                        // called on initial load and prevents direct navigation to a url for
                                        // a target, and also doesn't allow focusing of a newly created target
                                        // while an observation is selected. See https://app.shortcut.com/lucuma/story/4425/select-newly-created-target
                                        props.focusTargetId(none).unless_(rs.value.isEmpty)
                                      .mod(_)
                                  )
                                ),
                                TableStore(props.userId, TableId.TargetsSummary, cols)
                              )
        virtualizerRef   <- useRef(none[HTMLTableVirtualizer])
        resizer          <- useResizeDetector
        _                <- useEffectWithDeps(props.focusedTargetId, resizer): (focusedTargetId, _) =>
                              focusedTargetId.foldMap: targetId =>
                                virtualizerRef.get.flatMap: refOpt =>
                                  val focusedTargetIdStr: String = targetId.toString
                                  Callback:
                                    for
                                      virtualizer <- refOpt
                                      idx         <- table
                                                       .getRowModel()
                                                       .flatRows
                                                       .indexWhere(_.id.value === focusedTargetIdStr)
                                                       .some
                                                       .filterNot(_ == -1)
                                    yield virtualizer.scrollToIndex(idx + 1, ScrollOptions)
      yield
        def onTextChange(e: ReactEventFromInput): Callback = {
          val files = e.target.files.toList
          // set value to null so we can reuse the import button
          (Callback(e.target.value = null) *> filesToImport.set(files))
            .when_(files.nonEmpty)
        }

        val title =
          React.Fragment(
            <.div(ExploreStyles.TableSelectionToolbar)(
              React.Fragment(
                if (props.readonly) EmptyVdom
                else
                  React.Fragment(
                    HelpIcon("target/main/target-import.md".refined),
                    <.label(
                      PrimeStyles.Component |+| PrimeStyles.Button |+|
                        PrimeStyles.ButtonSmall |+| LucumaPrimeStyles.Compact |+| ExploreStyles.FileUpload,
                      ^.htmlFor := "target-import"
                    )(
                      Icons.FileArrowUp
                        .withClass(PrimeStyles.ButtonIcon |+| PrimeStyles.ButtonIconLeft),
                      "Import"
                    ),
                    <.input(
                      ^.tpe    := "file",
                      ^.onChange ==> onTextChange,
                      ^.id     := "target-import",
                      ^.name   := "file",
                      ^.accept := ".csv"
                    ),
                    TargetImportPopup(props.programId, filesToImport)
                  ),
                // props.toggleAllRowsSelected.map: toggleAllRowsSelected =>
                React.Fragment(
                  Button(
                    size = Button.Size.Small,
                    icon = Icons.CheckDouble,
                    label = "Select All",
                    clazz = LucumaPrimeStyles.Compact |+| ExploreStyles.CompactNowrap,
                    onClick = table.toggleAllRowsSelected(true)
                  ).compact,
                  Button(
                    size = Button.Size.Small,
                    icon = Icons.SquareXMark,
                    label = "Select None",
                    clazz = LucumaPrimeStyles.Compact |+| ExploreStyles.CompactNowrap,
                    onClick = props.focusTargetId(none) >> table.toggleAllRowsSelected(false)
                  ).compact
                ),
                EnumDropdownView(
                  id = "target-disposition".refined,
                  value = disposition
                )
              )
            ),
            ColumnSelectorInTitle(ColNames.toList, columnVisibility)
          )

        val body = PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          innerContainerMod = ^.width := "100%",
          containerRef = resizer.ref,
          tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
          headerCellMod = headerCell =>
            ColumnClasses
              .get(headerCell.column.id)
              .orEmpty |+| ExploreStyles.StickyHeader,
          rowMod = rowTagMod: row =>
            TagMod(
              ExploreStyles.TableRowSelected.when_(
                row.getIsSelected() || props.focusedTargetId.exists(_.toString === row.id.value)
              ),
              ^.onClick ==> table.getMultiRowSelectedHandler(row.id)
            ),
          cellMod = cellTagMod(cell => ColumnClasses.get(cell.column.id).orEmpty),
          virtualizerRef = virtualizerRef,
          emptyMessage = <.div(Constants.NoTargets)
          // workaround to redraw when files are imported
        ).withKey(s"summary-table-${filesToImport.get.size}")

        TileContents(title, body)
    })
