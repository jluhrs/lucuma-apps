// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

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
import explore.model.ConstraintGroup
import explore.model.ConstraintGroupList
import explore.model.ConstraintTabTileIds
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.AirMassBound
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.HourAngleBound
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.react.common.Css
import lucuma.react.table.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

final case class ConstraintsSummaryTile(
  userId:         Option[User.Id],
  programId:      Program.Id,
  constraintList: ConstraintGroupList,
  expandedIds:    View[SortedSet[ObsIdSet]],
  backButton:     VdomNode
) extends Tile[ConstraintsSummaryTile](
      id = ConstraintTabTileIds.Summary.id,
      title = "Constraints Summary",
      backButton.some,
      canMinimize = false,
      canMaximize = false
    )(ConstraintsSummaryTile)

object ConstraintsSummaryTile
    extends TileComponent[ConstraintsSummaryTile]((props, _) =>
      val EditColumnId: ColumnId         = ColumnId("edit")
      val IQColumnId: ColumnId           = ColumnId("iq")
      val CCColumnId: ColumnId           = ColumnId("cc")
      val BGColumnId: ColumnId           = ColumnId("bg")
      val WVColumnId: ColumnId           = ColumnId("wv")
      val MinAMColumnId: ColumnId        = ColumnId("minam")
      val MaxAMColumnId: ColumnId        = ColumnId("maxam")
      val MinHAColumnId: ColumnId        = ColumnId("minha")
      val MaxHAColumnId: ColumnId        = ColumnId("maxha")
      val CountColumnId: ColumnId        = ColumnId("count")
      val ObservationsColumnId: ColumnId = ColumnId("observations")

      val SelectableColumnNames: TreeSeqMap[ColumnId, String] =
        TreeSeqMap(
          IQColumnId           -> "IQ",
          CCColumnId           -> "CC",
          BGColumnId           -> "BG",
          WVColumnId           -> "WV",
          MinAMColumnId        -> "Min AM",
          MaxAMColumnId        -> "Max AM",
          MinHAColumnId        -> "Min HA",
          MaxHAColumnId        -> "Max HA",
          CountColumnId        -> "Count",
          ObservationsColumnId -> "Observations"
        )

      val ColumnNames: TreeSeqMap[ColumnId, String] =
        TreeSeqMap(EditColumnId -> " ") ++ SelectableColumnNames

      val DefaultColVisibility: ColumnVisibility =
        ColumnVisibility(
          MinAMColumnId -> Visibility.Hidden,
          MinHAColumnId -> Visibility.Hidden,
          MaxHAColumnId -> Visibility.Hidden
        )

      val ColDef = ColumnDef[ConstraintGroup]

      val ColumnClasses: Map[ColumnId, Css] = Map(
        EditColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.ConstraintsSummaryEdit)
      )

      def columns(props: ConstraintsSummaryTile, ctx: AppContext[IO]): List[ColDef.Type] =
        def column[V](id: ColumnId, accessor: ConstraintGroup => V): ColDef.TypeFor[V] =
          ColDef(id, accessor, ColumnNames(id))

        def goToObsSet(obsIdSet: ObsIdSet): Callback =
          ctx.pushPage(
            (AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet)).some
          )

        def obsSetUrl(obsIdSet: ObsIdSet): String =
          ctx.pageUrl(
            (AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet)).some
          )

        def goToObs(obsId: Observation.Id): Callback =
          ctx.pushPage(
            (AppTab.Constraints, props.programId, Focused.singleObs(obsId)).some
          )

        def obsUrl(obsId: Observation.Id): String =
          ctx.pageUrl(
            (AppTab.Constraints, props.programId, Focused.singleObs(obsId)).some
          )

        List(
          column(EditColumnId, ConstraintGroup.obsIds.get)
            .withCell(cell =>
              <.a(^.href := obsSetUrl(cell.value),
                  ^.onClick ==> (_.preventDefaultCB *> goToObsSet(cell.value)),
                  Icons.Edit
              )
            )
            .withEnableSorting(false),
          column(
            IQColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.imageQuality).get
          )
            .withCell(_.value.shortName)
            .sortableBy(_.shortName),
          column(
            CCColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.cloudExtinction).get
          )
            .withCell(_.value.shortName)
            .sortableBy(_.shortName),
          column(
            BGColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.skyBackground).get
          )
            .withCell(_.value.label)
            .sortableBy(_.label),
          column(
            WVColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.waterVapor).get
          )
            .withCell(_.value.label)
            .sortableBy(_.label),
          column(
            MinAMColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
          )
            .withCell(_.value match
              case ElevationRange.ByAirMass(min, _) => f"${min.value}%.1f"
              case ElevationRange.ByHourAngle(_, _) => "")
            .sortableBy(_ match
              case ElevationRange.ByAirMass(min, _) => min.toBigDecimal
              case ElevationRange.ByHourAngle(_, _) => AirMassBound.Min.toBigDecimal - 1),
          column(
            MaxAMColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
          )
            .withCell(_.value match
              case ElevationRange.ByAirMass(_, max) => f"${max.value}%.1f"
              case ElevationRange.ByHourAngle(_, _) => "")
            .sortableBy(_ match
              case ElevationRange.ByAirMass(_, max) => max.toBigDecimal
              case ElevationRange.ByHourAngle(_, _) => AirMassBound.Min.toBigDecimal - 1),
          column(
            MinHAColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
          )
            .withCell(_.value match
              case ElevationRange.ByAirMass(_, _)     => ""
              case ElevationRange.ByHourAngle(min, _) => f"${min.value}%.1f")
            .sortableBy(_ match
              case ElevationRange.ByAirMass(_, _)     => HourAngleBound.Min.toBigDecimal - 1
              case ElevationRange.ByHourAngle(min, _) => min.toBigDecimal),
          column(
            MaxHAColumnId,
            ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
          )
            .withCell(_.value match
              case ElevationRange.ByAirMass(_, _)     => ""
              case ElevationRange.ByHourAngle(_, max) => f"${max.value}%.1f")
            .sortableBy(_ match
              case ElevationRange.ByAirMass(_, _)     => HourAngleBound.Min.toBigDecimal - 1
              case ElevationRange.ByHourAngle(_, max) => max.toBigDecimal),
          column(CountColumnId, _.obsIds.length),
          column(ObservationsColumnId, ConstraintGroup.obsIds.get)
            .withCell(cell =>
              <.span(
                cell.value.toSortedSet.toList
                  .map(obsId =>
                    <.a(
                      ^.href := obsUrl(obsId),
                      ^.onClick ==> (_.preventDefaultCB
                        >> goToObs(obsId)
                        >> props.expandedIds.mod(_ + cell.value)
                        >> goToObsSet(ObsIdSet.one(obsId))),
                      obsId.toString
                    )
                  )
                  .mkReactFragment(", ")
              )
            )
            .withEnableSorting(false)
        )

      for {
        ctx              <- useContext(AppContext.ctx)
        columnVisibility <- useStateView(DefaultColVisibility)
        cols             <- useMemo(()): // Cols never changes, but needs access to props
                              _ => columns(props, ctx)
        // Memo rows
        rows             <-
          useMemo(props.constraintList):
            _.map(ConstraintGroup.fromTuple).toList.sortBy(_.constraintSet.summaryString)
        table            <- useReactTableWithStateStore:
                              import ctx.given

                              TableOptionsWithStateStore(
                                TableOptions(
                                  cols,
                                  rows,
                                  getRowId = (row, _, _) => RowId(row.constraintSet.toString),
                                  enableSorting = true,
                                  enableColumnResizing = true,
                                  columnResizeMode = ColumnResizeMode.OnChange,
                                  state = PartialTableState(columnVisibility = columnVisibility.get),
                                  onColumnVisibilityChange = stateInViewHandler(columnVisibility.mod)
                                ),
                                TableStore(props.userId, TableId.ConstraintsSummary, cols)
                              )
      } yield TileContents:
        <.div(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            tableMod = ExploreStyles.ExploreTable,
            emptyMessage = <.div("No constraints present"),
            headerCellMod = headerCell =>
              ColumnClasses
                .get(headerCell.column.id)
                .orEmpty |+| ExploreStyles.StickyHeader,
            cellMod = cellTagMod(cell => ColumnClasses.get(cell.column.id).orEmpty)
          )
        )
    )
