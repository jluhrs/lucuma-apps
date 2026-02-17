// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.UserPreferencesQueries.ObservationPreferences
import explore.components.*
import explore.components.ColumnSelectorInTitle
import explore.components.ui.ExploreStyles
import explore.config.ObsTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.BlindOffset
import explore.model.GuideStarSelection
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsIdSetEditInfo
import explore.model.ObservationTargets
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.TargetList
import explore.model.UserPreferences
import explore.model.enums.TileSizeState
import explore.model.reusability.given
import explore.services.OdbObservationApi
import explore.targets.TargetColumns
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.TargetWithId
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.collection.immutable.SortedSet

final case class ObservationTargetsEditorTile(
  userId:              Option[User.Id],
  tileId:              Tile.TileId,
  programId:           Program.Id,
  programType:         ProgramType,
  obsIds:              ObsIdSet,
  obsAndTargets:       UndoSetter[ObservationsAndTargets],
  obsTime:             View[Option[Instant]],
  obsDuration:         View[Option[TimeSpan]],
  obsConf:             ObsConfiguration,
  digest:              CalculatedValue[Option[ExecutionDigest]],
  focusedTargetId:     Option[Target.Id],
  setTarget:           (Option[Target.Id], SetRouteVia) => Callback,
  onCloneTarget:       OnCloneParameters => Callback,
  onAsterismUpdate:    OnAsterismUpdateParams => Callback,
  obsInfo:             Target.Id => TargetEditObsInfo,
  searching:           View[Set[Target.Id]],
  titleText:           String,
  userPreferences:     View[UserPreferences],
  guideStarSelection:  View[GuideStarSelection],
  attachments:         View[AttachmentList],
  authToken:           Option[NonEmptyString],
  readonly:            Boolean,
  allowEditingOngoing: Boolean,
  isStaffOrAdmin:      Boolean,
  sequenceChanged:     Callback = Callback.empty,
  blindOffsetInfo:     Option[(Observation.Id, View[BlindOffset])] = None,
  backButton:          Option[VdomNode] = None
)(using val odbApi: OdbObservationApi[IO])(using val logger: Logger[IO])
    extends Tile[ObservationTargetsEditorTile](
      tileId,
      titleText,
      renderBackButton = backButton,
      bodyClass = ExploreStyles.TargetTileBody,
      controllerClass = ExploreStyles.TargetTileController
    )(ObservationTargetsEditorTile):
  val allTargets: UndoSetter[TargetList] = obsAndTargets.zoom(ObservationsAndTargets.targets)
  val prefTargetId: Option[Target.Id]    =
    obsIds.single.flatMap(userPreferences.get.observationPreferences.get)

object ObservationTargetsEditorTile
    extends TileComponent[ObservationTargetsEditorTile](
      { (props, tileSize) =>
        import props.given

        // Save the time here. this works for the obs and target tabs
        // It's OK to save the viz time for executed observations, I think.
        val obsTimeView: View[Instant] =
          View(
            props.obsTime.get.getOrElse(Instant.now),
            (f, cb) =>
              val oldValue = props.obsTime.get
              val newValue = f(oldValue.getOrElse(Instant.now)).some
              props.obsTime.set(newValue) >>
                cb(oldValue.getOrElse(Instant.now), newValue.getOrElse(Instant.now))
          ).withOnMod: ct =>
            Callback.log(s"to the db $ct") *>
              props.odbApi.updateVisualizationTime(props.obsIds.toList, ct.some).runAsync

        val obsDurationView: View[Option[TimeSpan]] =
          props.obsDuration.withOnMod: t =>
            props.odbApi.updateVisualizationDuration(props.obsIds.toList, t).runAsync

        val obsTimeAndDurationView: View[(Instant, Option[TimeSpan])] =
          View(
            (props.obsTime.get.getOrElse(Instant.now), props.obsDuration.get),
            (mod, cb) =>
              val oldValue = (props.obsTime.get.getOrElse(Instant.now), props.obsDuration.get)
              val newValue = mod(oldValue)
              props.obsTime.set(newValue._1.some) >> props.obsDuration
                .set(newValue._2) >> cb(oldValue, newValue)
          ).withOnMod: tuple =>
            props.odbApi
              .updateVisualizationTimeAndDuration(props.obsIds.toList, tuple._1.some, tuple._2)
              .runAsync

        for
          ctx              <- useContext(AppContext.ctx)
          columnVisibility <- useStateView(TargetColumns.DefaultVisibility)
          // obsEditInfo <- useStateView[Option[ObsIdSetEditInfo]](none)
          adding           <- useStateView(AreAdding(false))

          obsEditInfo  <- useMemo((props.obsIds, props.obsAndTargets.get._1)):
                            ObsIdSetEditInfo.fromObservationList
          // _            <- useLayoutEffectWithDeps(obsEditInfo): roei =>
          //                   props.obsEditInfo.set(roei.value.some)
          observations <- useMemo((props.obsIds, props.obsAndTargets.get._1)): (ids, obses) =>
                            ids.idSet.toList.map(obses.get).flatten
          scienceIds   <- useMemo(observations): os =>
                            // all of the selected observations must have the same asterism
                            os.headOption.fold(SortedSet.empty[Target.Id])(_.scienceTargetIds)
          distinctSite <- useMemo(observations):
                            _.value.map(_.site) match
                              case head :: Nil => head
                              case _           => none
          // Build asterism IDs that include blind offset
          targetIds    <- useMemo(
                            (scienceIds, props.blindOffsetInfo.flatMap(_._2.get.blindOffsetTargetId))
                          ): (scienceIds, oBlindId) =>
                            // Include blind offset target in the IDs if present
                            scienceIds.value ++ oBlindId.toList
          obsTargets   <- useMemo((targetIds, props.allTargets.get)): (ids, targets) =>
                            ObservationTargets.fromIdsAndTargets(ids.value, targets)
          _            <- useLayoutEffectWithDeps(
                            (targetIds.value.toList, props.focusedTargetId, props.prefTargetId)
                          ): (allTargetIds, focusedTargetId, preferredTargetOpt) =>
                            // If the selected targetId is None, or not in the target list, we need
                            // to select one. Firt select the preferred target if saved or else
                            // the first science target. If no science target just pick the first overall target
                            // Need to replace history here.
                            focusedTargetId.filter(allTargetIds.contains) match
                              case None =>
                                val preferred = preferredTargetOpt.filter(allTargetIds.contains)
                                val tid       =
                                  preferred.orElse(scienceIds.value.headOption).orElse(allTargetIds.headOption)
                                props.setTarget(tid, SetRouteVia.HistoryReplace)
                              case _    => Callback.empty
          fullScreen   <- useStateView(AladinFullScreen.Normal)
        yield
          import ctx.given

          val obsTimeEditor = ObsTimeEditor(
            obsTimeView,
            obsDurationView,
            obsTimeAndDurationView,
            props.digest,
            props.obsIds.size > 1
          )

          val title =
            <.div(
              ExploreStyles.AsterismEditorTileTitle,
              if (tileSize.isMinimized)
                obsTimeEditor
              else
                React.Fragment(
                  // only pass in the unexecuted observations. Will be readonly if there aren't any
                  <.span(
                    AddTargetButton(
                      "Target",
                      props.programId,
                      obsEditInfo.unExecuted.getOrElse(props.obsIds),
                      props.obsAndTargets,
                      adding,
                      props.onAsterismUpdate,
                      readOnly = props.readonly || obsEditInfo.allAreExecuted,
                      allowBlindOffset = !props.readonly,
                      buttonClass = ExploreStyles.AddTargetButton,
                      blindOffsetInfo = props.blindOffsetInfo
                    )
                  ),
                  obsTimeEditor,
                  <.span(^.textAlign.right)(
                    ColumnSelectorInTitle(TargetColumns.AllColNames.toList, columnVisibility)
                  )
                )
            )

          val selectedTargetView: View[Option[Target.Id]] =
            View(
              props.focusedTargetId,
              (mod, cb) =>
                val oldValue = props.focusedTargetId
                val newValue = mod(props.focusedTargetId)
                props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(oldValue, newValue)
            ).withOnMod: tid =>
              props.obsIds.single.traverse_ : obsId =>
                ObservationPreferences.upsertPreferredTarget[IO](obsId, tid).runAsync

          val editWarningMsg: Option[String] =
            if (obsEditInfo.allAreExecuted)
              if (obsEditInfo.editing.length > 1)
                "All of the current observations are executed. Asterism is readonly.".some
              else "The current observation has been executed. Asterism is readonly.".some
            else if (obsEditInfo.executed.isDefined)
              "Adding and removing targets will only affect the unexecuted observations.".some
            else none

          val body =
            <.div(ExploreStyles.AladinFullScreen.when(fullScreen.get.value))(
              editWarningMsg.map(msg => <.div(ExploreStyles.SharedEditWarning, msg)),
              // the 'getOrElse doesn't matter. Controls will be readonly if all are executed
              TargetTable(
                props.userId,
                props.programId,
                obsEditInfo.unExecuted.getOrElse(props.obsIds),
                obsTargets,
                props.obsAndTargets,
                selectedTargetView,
                props.onAsterismUpdate,
                props.obsTime.get,
                distinctSite,
                fullScreen.get,
                props.readonly || obsEditInfo.allAreExecuted,
                props.blindOffsetInfo.map(_._2),
                columnVisibility
              ),
              (ObservationTargets.fromIdsAndTargets(targetIds, props.allTargets.get),
               props.focusedTargetId
              )
                .mapN: (targets, focusedTargetId) =>
                  val selectedTargetOpt: Option[UndoSetter[TargetWithId]] =
                    props.allTargets.zoom(Iso.id[TargetList].index(focusedTargetId))

                  val obsInfo = props.obsInfo(focusedTargetId)

                  <.div(
                    ExploreStyles.TargetTileEditor,
                    (selectedTargetOpt, props.userId).mapN: (targetWithId, userId) =>
                      TargetEditor(
                        props.programId,
                        props.programType,
                        userId,
                        targetWithId,
                        props.obsAndTargets,
                        targets.focusOn(focusedTargetId),
                        props.obsTime.get,
                        props.obsConf.some,
                        props.searching,
                        onClone = props.onCloneTarget,
                        obsInfo = obsInfo,
                        fullScreen = fullScreen,
                        userPreferences = props.userPreferences,
                        guideStarSelection = props.guideStarSelection,
                        attachments = props.attachments,
                        authToken = props.authToken,
                        readonly = props.readonly,
                        allowEditingOngoing = props.allowEditingOngoing,
                        isStaffOrAdmin = props.isStaffOrAdmin,
                        invalidateSequence = props.sequenceChanged,
                        blindOffsetInfo = props.blindOffsetInfo
                      )
                  ).some
            )

          TileContents(title, body)
      }
    )
