// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.BlindOffset
import explore.model.Constants
import explore.model.ObsIdSet
import explore.model.ObservationRegionsOrCoordinatesAt
import explore.model.ObservationTargets
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.services.OdbAsterismApi
import explore.services.OdbObservationApi
import explore.targets.MotionCorrectedTarget
import explore.targets.TargetColumns
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.ui.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import org.typelevel.log4cats.Logger

import java.time.Instant

case class TargetTable(
  userId:           Option[User.Id],
  programId:        Program.Id,
  obsIds:           ObsIdSet, // Only used to invoke DB - should only be unexecuted observations
  // Targets are not modified here, we only modify which ones belong to the ObservationTargets.
  obsTargets:       Option[ObservationTargets],
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  selectedTarget:   View[Option[Target.Id]],
  onAsterismUpdate: OnAsterismUpdateParams => Callback,
  vizTime:          Option[Instant],
  site:             Option[Site],
  fullScreen:       AladinFullScreen,
  readOnly:         Boolean,
  blindOffset:      Option[View[BlindOffset]] = None,
  columnVisibility: View[ColumnVisibility]
) extends ReactFnProps(TargetTable.component)

object TargetTable:
  private type Props = TargetTable

  case class TableMeta(
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )

  private val ColDef = ColumnDef[MotionCorrectedTarget].WithTableMeta[TableMeta]

  private val DeleteColumnId: ColumnId = ColumnId("delete")

  val ColumnNames: Map[ColumnId, String] = Map(DeleteColumnId -> " ") ++ TargetColumns.AllColNames

  private val ColumnClasses: Map[ColumnId, Css] = Map(
    DeleteColumnId             -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryDelete),
    TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
    TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
  )

  private def deleteTarget(
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    target:           TargetWithId,
    onAsterismUpdate: OnAsterismUpdateParams => Callback,
    blindOffset:      Option[View[BlindOffset]]
  )(using OdbAsterismApi[IO] & OdbObservationApi[IO], Logger[IO]): Callback =
    if (target.disposition === TargetDisposition.BlindOffset)
      // will only have a blind offset for a single observation
      deleteBlindOffsetTarget(obsIds.head, blindOffset)
    else deleteAsterismTarget(obsIds, obsAndTargets, target, onAsterismUpdate)

  private def deleteBlindOffsetTarget(
    obsId:       Observation.Id,
    blindOffset: Option[View[BlindOffset]]
  )(using api: OdbObservationApi[IO], logger: Logger[IO]): Callback =
    // blind offsets are not displayed in the target list, so no need to proactively delete them
    // from the TargetList
    ConfirmDialog.confirmDialog(
      message = <.div("Delete the blind offset? This action cannot be undone."),
      header = "Blind offset delete",
      acceptLabel = "Yes, delete",
      accept = api.deleteBlindOffsetTarget(obsId).runAsync >>
        blindOffset.foldMap(_.set(BlindOffset(false, None, BlindOffsetType.Automatic))),
      position = DialogPosition.Top,
      acceptClass = PrimeStyles.ButtonSmall,
      rejectClass = PrimeStyles.ButtonSmall,
      icon = Icons.SkullCrossBones(^.color.red)
    )

  private def deleteAsterismTarget(
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    target:           TargetWithId,
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )(using OdbAsterismApi[IO]): Callback =
    AsterismActions
      .removeTargetFromAsterisms(target, obsIds, onAsterismUpdate)
      .set(obsAndTargets)(true) >>
      // the ".async.toCallback" seems to let the model update before we try changing the UI
      onAsterismUpdate(OnAsterismUpdateParams(target.id, obsIds, false, false)).async.toCallback

  protected val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx     <- useContext(AppContext.ctx)
        cols    <- useMemo(props.readOnly): readOnly =>
                     import ctx.given

                     Option
                       .unless(readOnly)(
                         ColDef(
                           DeleteColumnId,
                           _.id,
                           "",
                           cell =>
                             Button(
                               text = true,
                               clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
                               icon = Icons.Trash,
                               tooltip = "Delete",
                               onClickE = (e: ReactMouseEvent) =>
                                 e.preventDefaultCB >>
                                   e.stopPropagationCB >>
                                   cell.table.options.meta.foldMap(m =>
                                     deleteTarget(
                                       m.obsIds,
                                       m.obsAndTargets,
                                       cell.row.original.targetWithId,
                                       m.onAsterismUpdate,
                                       props.blindOffset
                                     )
                                   )
                             ).tiny.compact,
                           size = 35.toPx,
                           enableSorting = false
                         )
                       )
                       .toList ++
                       TargetColumns.Builder.ForProgram(ColDef, _.regionOrCoords).AllColumns
        vizTime <- useEffectKeepResultWithDeps(props.vizTime): vizTime =>
                     IO(vizTime.getOrElse(Instant.now()))
        rowsPot <-
          useEffectKeepResultWithDeps((vizTime.value.toOption, props.obsTargets, props.site)):
            (vt, optObsTargets, site) =>
              import ctx.given

              optObsTargets.foldMap: obsTargets =>
                ObservationRegionsOrCoordinatesAt
                  .build(obsTargets, vt, site)
                  .map: rorc =>
                    // we want the blind offset last (sc-7428)
                    (rorc.science ++ rorc.blindOffset.toList).map(MotionCorrectedTarget.apply)
        table   <- useReactTableWithStateStore:
                     import ctx.given

                     TableOptionsWithStateStore(
                       TableOptions(
                         cols,
                         rowsPot.value.map(_.toOption.orEmpty),
                         getRowId = (row, _, _) => RowId(row.id.toString),
                         enableSorting = true,
                         enableColumnResizing = true,
                         columnResizeMode = ColumnResizeMode.OnChange,
                         state = PartialTableState(columnVisibility = props.columnVisibility.get),
                         onColumnVisibilityChange = stateInViewHandler(props.columnVisibility.mod),
                         meta = TableMeta(props.obsIds, props.obsAndTargets, props.onAsterismUpdate)
                       ),
                       TableStore(props.userId, TableId.AsterismTargets, cols)
                     )
        adding  <- useStateView(AreAdding(false))
      yield
        if (rowsPot.value.map(_.toOption.orEmpty).isEmpty)
          if (props.readOnly)
            <.div(LucumaStyles.HVCenter)(Constants.NoTargets)
          else
            <.div(LucumaStyles.HVCenter)(
              AddTargetButton(
                "Add a target",
                props.programId,
                props.obsIds,
                props.obsAndTargets,
                adding,
                props.onAsterismUpdate,
                buttonClass = LucumaPrimeStyles.Massive
              )
            )
        else
          <.div(ExploreStyles.ExploreTable |+| ExploreStyles.AsterismTable)(
            PrimeTable(
              table,
              striped = true,
              compact = Compact.Very,
              tableMod = ExploreStyles.ExploreTable,
              headerCellMod = headerCell =>
                ColumnClasses
                  .get(headerCell.column.id)
                  .orEmpty |+| ExploreStyles.StickyHeader,
              rowMod = rowTagMod: row =>
                TagMod(
                  ExploreStyles.TableRowSelected
                    .when_(props.selectedTarget.get.exists(_ === row.original.id)),
                  ^.onClick --> props.selectedTarget.set(row.original.id.some)
                ),
              cellMod = cellTagMod(cell => ColumnClasses.get(cell.column.id).orEmpty)
            )
          )
