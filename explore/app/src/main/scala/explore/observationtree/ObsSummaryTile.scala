// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.events.HorizonsMessage
import explore.model.AppContext
import explore.model.Constants
import explore.model.Group
import explore.model.GroupList
import explore.model.ObsSummaryTabTileIds
import explore.model.Observation
import explore.model.ObservationList
import explore.model.ObservationRegionsOrCoordinatesAt
import explore.model.ObservationTargets
import explore.model.TargetList
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.primereact.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.TargetWithId
import lucuma.ui.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Iso
import workers.WorkerClient

import java.time.Instant

final case class ObsSummaryTile(
  userId:          Option[User.Id],
  programId:       Program.Id,
  observations:    UndoSetter[ObservationList],
  selectedObsIds:  View[List[Observation.Id]],
  groups:          View[GroupList],
  allTargets:      TargetList,
  showScienceBand: Boolean,
  readonly:        Boolean,
  backButton:      VdomNode
) extends Tile[ObsSummaryTile](
      ObsSummaryTabTileIds.SummaryId.id,
      "Observations Summary",
      renderBackButton = backButton.some,
      canMinimize = false,
      canMaximize = false
    )(ObsSummaryTile)

object ObsSummaryTile
    extends TileComponent[ObsSummaryTile]({ (props, _) =>
      import ObsSummaryColumns.*

      def obsGroup(groupId: Option[Group.Id], groups: GroupList): Option[Group] =
        groupId
          .flatMap(id => props.groups.get.get(id))
          .flatMap:
            case g if g.isTelluricCalibration => obsGroup(g.parentId, groups)
            case g                            => g.some

      def getObsRows(
        obs:      Observation,
        targets:  List[TargetWithId],
        obsGroup: Option[Group],
        now:      Instant // used if there is no observation time
      )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Expandable[ObsSummaryRow]] =
        val optObsTargets = ObservationTargets.fromTargets(targets)
        optObsTargets
          .fold(ObservationRegionsOrCoordinatesAt.Empty.pure[IO]): obsTargets =>
            ObservationRegionsOrCoordinatesAt.build(
              obsTargets,
              obs.observationTime.getOrElse(now).some,
              obs.observingMode.map(_.siteFor)
            )
          .map: regionsOrCoords =>
            val headTarget = regionsOrCoords.science.headOption.map(_._1)
            Expandable(
              ObsSummaryRow
                .ObsRow(obs, headTarget, optObsTargets, regionsOrCoords.asterism, obsGroup),
              if regionsOrCoords.science.size > 1 then
                regionsOrCoords.science.map: (twid, rorcs) =>
                  Expandable(
                    ObsSummaryRow.ExpandedTargetRow(obs, twid, rorcs)
                  )
              else Nil
            )

      for
        ctx                   <- useContext(AppContext.ctx)
        columnVisibility      <- useStateView[ColumnVisibility](DefaultColVisibility)
        toggleAllRowsSelected <- useStateView[Option[Boolean => Callback]](none)

        cols    <- useMemo(()): // Columns
                     _ => columns(props.programId, ctx)
        rowsPot <- useEffectKeepResultWithDeps(
                     (props.observations.get.values.toList, props.allTargets, props.groups.get)
                   ): (obsList, allTargets, groups) =>
                     import ctx.given

                     IO.now()
                       .flatMap: now =>
                         obsList
                           .filterNot(_.isCalibration)
                           .traverse: obs =>
                             val targets = obs.scienceTargetIds.toList
                               .map(id => allTargets.get(id))
                               .flattenOption
                             val group   = obsGroup(obs.groupId, groups)
                             getObsRows(obs, targets, group, now)

        table   <- useReactTableWithStateStore:
                     import ctx.given

                     val obsIds2RowSelection: Iso[List[Observation.Id], RowSelection] =
                       Iso[List[Observation.Id], RowSelection](obsIds =>
                         RowSelection:
                           obsIds.map(obsId => RowId(obsId.toString) -> true).toMap
                       )(selection =>
                         selection.value
                           .filter(_._2)
                           .keys
                           .toList
                           .map(rowId => Observation.Id.parse(rowId.value))
                           .flattenOption
                       )

                     val rowSelection: View[RowSelection] =
                       props.selectedObsIds.as(obsIds2RowSelection)

                     TableOptionsWithStateStore(
                       TableOptions(
                         cols,
                         rowsPot.value.map(_.toOption.orEmpty),
                         enableExpanding = true,
                         getSubRows = (row, _) => row.subRows,
                         getRowId = (row, _, _) =>
                           RowId:
                             row.value.fold(
                               o => o.obs.id.toString + o.targetWithId.id.toString,
                               _.obs.id.toString
                             )
                         ,
                         enableSorting = true,
                         enableMultiRowSelection = true,
                         state = PartialTableState(
                           rowSelection = rowSelection.get,
                           columnVisibility = columnVisibility.get
                         ),
                         onRowSelectionChange = stateInViewHandler(rowSelection.mod),
                         onColumnVisibilityChange = stateInViewHandler(columnVisibility.mod)
                       ),
                       TableStore(
                         props.userId,
                         TableId.ObservationsSummary,
                         cols,
                         ColumnsExcludedFromVisibility
                       )
                     )
        _       <- useEffectOnMount:
                     toggleAllRowsSelected.set: // TODO Can this whole dance be avoided now?
                       ((v: Boolean) => table.toggleAllRowsSelected(v)).some
        _       <- useEffectWithDeps(props.showScienceBand): showScienceBand =>
                     table
                       .getColumn(ScienceBandColumnId.value)
                       .foldMap(_.toggleVisibility(showScienceBand))
        resizer <- useResizeDetector
        adding  <- useStateView(AddingObservation(false)) // adding new observation
      yield
        val title = React.Fragment(
          toggleAllRowsSelected.get.map: toggleAllRowsSelected =>
            <.span(^.textAlign.center)(
              Button(
                size = Button.Size.Small,
                icon = Icons.CheckDouble,
                label = "Select All",
                onClick = toggleAllRowsSelected(true)
              ).compact,
              Button(
                size = Button.Size.Small,
                icon = Icons.SquareXMark,
                label = "Select None",
                onClick = toggleAllRowsSelected(false)
              ).compact
            ),
          ColumnSelectorInTitle(SelectableColumnNames, columnVisibility)
        )

        val body = PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          innerContainerMod = ^.width := "100%",
          containerRef = resizer.ref,
          hoverableRows = rowsPot.value.value.toOption.exists(_.nonEmpty),
          tableMod =
            ExploreStyles.ExploreTable |+| ExploreStyles.ObservationsSummaryTable |+| ExploreStyles.ExploreSelectableTable,
          headerCellMod = _ => ExploreStyles.StickyHeader,
          rowMod = rowTagMod: row =>
            TagMod(
              ExploreStyles.TableRowSelected
                .when(row.getIsSelected() && (row.subRows.isEmpty || !row.getIsExpanded())),
              ExploreStyles.TableRowSelectedStart
                .when(row.getIsSelected() && row.subRows.nonEmpty && row.getIsExpanded()),
              ExploreStyles.TableRowSelectedSpan
                .when:
                  props.selectedObsIds.get.contains_(row.original.value.obs.id)
              ,
              ExploreStyles.TableRowSelectedEnd.when:
                row.original.value.isLastAsterismTargetOf
                  .exists(props.selectedObsIds.get.contains_)
              ,
              ^.onClick ==> table
                .getMultiRowSelectedHandler(RowId(row.original.value.obs.id.toString))
            ),
          emptyMessage =
            if (props.readonly)
              <.div(Constants.NoObservations)
            else
              <.span(LucumaStyles.HVCenter)(
                Button(
                  severity = Button.Severity.Success,
                  icon = Icons.New,
                  disabled = adding.get.value,
                  loading = adding.get.value,
                  label = "Add an observation",
                  clazz = LucumaPrimeStyles.Massive |+| ExploreStyles.ObservationsSummaryAdd,
                  onClick = insertObs(props.programId,
                                      none,
                                      props.observations,
                                      adding,
                                      ctx
                  ).runAsyncAndForget
                ).tiny.compact
              )
        )

        TileContents(title, body)
    })
