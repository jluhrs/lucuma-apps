// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.BlindOffset
import explore.model.EmptyOpportunityTarget
import explore.model.EmptySiderealTarget
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.PopupState
import explore.model.TargetList
import explore.services.OdbAsterismApi
import explore.services.OdbObservationApi
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.SplitButton
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.ui.primereact.*

case class AddTargetButton(
  label:            String,
  programId:        Program.Id,
  obsIds:           ObsIdSet,
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  adding:           View[AreAdding],
  onAsterismUpdate: OnAsterismUpdateParams => Callback,
  readOnly:         Boolean = false,
  allowBlindOffset: Boolean = false, // will be staff only for Ongoing
  buttonClass:      Css = Css.Empty,
  blindOffsetInfo:  Option[(Observation.Id, View[BlindOffset])] = none
) extends ReactFnProps(AddTargetButton):
  val targetList: View[TargetList] = obsAndTargets.model.zoom(ObservationsAndTargets.targets)

object AddTargetButton
    extends ReactFnComponent[AddTargetButton](props =>

      def updateTargetList(
        toDelete: Option[Target.Id],
        toAdd:    Option[TargetWithId]
      ): Callback =
        props.targetList.mod: tl =>
          val removed = toDelete.fold(tl)(tl.removed)
          toAdd.fold(removed)(a => removed.updated(a.id, a))

      def insertTarget(
        programId:        Program.Id,
        obsIds:           ObsIdSet,
        obsAndTargets:    UndoSetter[ObservationsAndTargets],
        targetWithOptId:  TargetWithOptId,
        onAsterismUpdate: OnAsterismUpdateParams => Callback
      )(using odbApi: OdbTargetApi[IO] & OdbAsterismApi[IO]): IO[Unit] =
        targetWithOptId.optId
          .fold(
            odbApi
              .insertTarget(programId, targetWithOptId.target)
              .map((_, true))
          )(id => IO((id, false)))
          .flatMap((id, created) =>
            (AsterismActions
              .addTargetToAsterisms(
                targetWithOptId.withId(id),
                obsIds,
                created,
                onAsterismUpdate
              )
              .set(obsAndTargets)(false) >>
              // Do the first onAsterismUpdate here so it is synchronous with the setter in the Action.
              // the ".async.toCallback" seems to let the model update before we try changing the UI
              onAsterismUpdate(
                OnAsterismUpdateParams(id, obsIds, true, true)
              ).async.toCallback).toAsync
          )

      def insertManualBlindOffset(
        obsId:           Observation.Id,
        targetWithOptId: TargetWithOptId,
        blindOffset:     View[BlindOffset]
      )(using api: OdbObservationApi[IO]): IO[Unit] =
        api
          .setBlindOffsetTarget(obsId, targetWithOptId.target, BlindOffsetType.Manual)
          .flatMap(id =>
            (updateTargetList(
              blindOffset.get.blindOffsetTargetId,
              targetWithOptId.withId(id).some
            ) >>
              blindOffset.set(BlindOffset(true, id.some, BlindOffsetType.Manual))).toAsync
          )

      for
        ctx        <- useContext(AppContext.ctx)
        popupState <- useStateView(PopupState.Closed)
        onSelected <- useStateView((_: TargetWithOptId) => Callback.empty)
        sources    <- useStateView:
                        // we'll always set this before opening the popup
                        NonEmptyList
                          .one[TargetSource[IO]](
                            TargetSource.FromProgram[IO](props.obsAndTargets.get._2)
                          )
        blindRef   <- usePopupMenuRef
      yield
        import ctx.given

        def insertTargetCB(targetWithOptId: TargetWithOptId): Callback =
          insertTarget(props.programId,
                       props.obsIds,
                       props.obsAndTargets,
                       targetWithOptId,
                       props.onAsterismUpdate
          )
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        def insertManualBlindOffsetCB(obsId: Observation.Id, blindOffset: View[BlindOffset])(
          targetWithOptId: TargetWithOptId
        ): Callback =
          // the search returns a science target
          val targetAsBlind = targetWithOptId.copy(disposition = TargetDisposition.BlindOffset)
          insertManualBlindOffset(
            obsId,
            targetAsBlind,
            blindOffset
          )
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        def initializeAutomaticBlindOffsetCB(
          obsId:       Observation.Id,
          blindOffset: View[BlindOffset]
        ): Callback =
          (ctx.odbApi
            .initializeAutomaticBlindOffset(obsId) >>
            (updateTargetList(
              blindOffset.get.blindOffsetTargetId,
              none
            ) >>
              blindOffset
                .set(
                  BlindOffset(true, none, BlindOffsetType.Automatic)
                )).toAsync)
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        val observations: List[Observation] =
          props.obsIds.toList.map(props.obsAndTargets.get._1.get).flattenOption

        // all observations have the same science targets, but that's not true of blind offsets
        val hasTargets: Boolean =
          observations.headOption.forall(_.scienceTargetIds.nonEmpty) || observations.exists(
            _.blindOffset.useBlindOffset
          )

        val hasTargetOfOpportunity: Boolean =
          observations.headOption.forall(_.hasTargetOfOpportunity(props.targetList.get))

        val programsAndSimbad = NonEmptyList.of(
          TargetSource.FromProgram[IO](props.obsAndTargets.get._2, filterToOs = hasTargets),
          TargetSource.FromSimbad[IO](ctx.simbadClient)
        )

        val simbad = NonEmptyList.one(
          TargetSource.FromSimbad[IO](ctx.simbadClient)
        )

        val horizons = NonEmptyList.one(
          TargetSource.FromHorizons[IO](ctx.horizonsClient)
        )

        val blindOffsetItems: List[MenuItem] =
          props.blindOffsetInfo
            .fold(List.empty)((obsId, blindOffset) =>
              List(
                Option.unless(blindOffset.get.isAutomatic)(
                  MenuItem.Item(
                    "Automatic Blind Offset",
                    icon = Icons.LocationDot,
                    command = initializeAutomaticBlindOffsetCB(obsId, blindOffset)
                  )
                ),
                MenuItem
                  .Item(
                    "Blind Offset Search",
                    icon = Icons.LocationDot,
                    command = onSelected.set(insertManualBlindOffsetCB(obsId, blindOffset)) >>
                      sources.set(simbad) >> popupState.set(PopupState.Open)
                  )
                  .some,
                MenuItem
                  .Item(
                    "Empty Blind Offset",
                    icon = Icons.LocationDot,
                    command = insertManualBlindOffsetCB(obsId, blindOffset)(
                      TargetWithOptId(None,
                                      EmptySiderealTarget,
                                      TargetDisposition.BlindOffset,
                                      None
                      )
                    )
                  )
                  .some
              ).flattenOption
            )
        val showBlindOffsetButton            =
          props.readOnly && props.allowBlindOffset && blindOffsetItems.nonEmpty

        val menuItems = List(
          MenuItem.Item("Non-Sidereal Target Search",
                        icon = Icons.PlanetRinged,
                        command = onSelected.set(insertTargetCB) >>
                          sources.set(horizons) >> popupState.set(PopupState.Open)
          ),
          MenuItem.Item("Empty Sidereal Target",
                        icon = Icons.Star,
                        command = insertTargetCB(TargetWithOptId.newScience(EmptySiderealTarget))
          ),
          MenuItem.Item("Target of Opportunity",
                        icon = Icons.HourglassClock,
                        command =
                          insertTargetCB(TargetWithOptId.newScience(EmptyOpportunityTarget)),
                        disabled = hasTargets
          )
        ) ++
          blindOffsetItems

        // In order for the title bar to look right, we need to have exactly one button in the DOM,
        // although it doesn't need to be visible.
        val button: VdomNode =
          if showBlindOffsetButton then
            Button(
              label = "Blind Offset",
              icon = Icons.New,
              severity = Button.Severity.Success,
              loading = props.adding.get.value,
              onClickE = blindRef.toggle,
              clazz = props.buttonClass
            ).tiny.compact
          else
            SplitButton(
              model = menuItems,
              onClick = onSelected.set(insertTargetCB) >> sources.set(programsAndSimbad) >>
                popupState.set(PopupState.Open),
              severity = Button.Severity.Success,
              icon = Icons.New,
              disabled = props.readOnly || props.adding.get.value || hasTargetOfOpportunity,
              loading = props.adding.get.value,
              label = props.label,
              clazz = props.buttonClass |+| ExploreStyles.Hidden.when_(props.readOnly)
            ).tiny.compact

        React.Fragment(
          button,
          TargetSelectionPopup(
            "Add Target",
            popupState,
            sources.get,
            selectExistingLabel = "Link",
            selectExistingIcon = Icons.Link,
            selectNewLabel = "Add",
            selectNewIcon = Icons.New,
            onSelected = onSelected.get
          ),
          PopupMenu(model = blindOffsetItems).withRef(blindRef.ref)
        )
    )
