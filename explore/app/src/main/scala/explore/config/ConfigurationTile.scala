// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Order.given
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.common.Aligner
import explore.common.ScienceQueries.ScienceRequirementsUndoView
import explore.common.ScienceQueries.UpdateScienceRequirements
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsConfiguration
import explore.model.ObsIdSetEditInfo
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.ObservingModeGroupList
import explore.model.ObservingModeSummary
import explore.model.PosAngleConstraintAndObsMode
import explore.model.ScienceRequirements
import explore.model.TargetList
import explore.model.enums.PosAngleOptions
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.model.syntax.all.*
import explore.modes.ConfigSelection
import explore.modes.ItcInstrumentConfig
import explore.modes.ScienceModes
import explore.services.OdbObservationApi
import explore.syntax.ui.*
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.core.util.Timestamp
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.ObservingMode.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.schemas.itc.syntax.*

import scala.collection.immutable.SortedSet

final case class ConfigurationTile(
  userId:                   Option[User.Id],
  programId:                Program.Id,
  obsId:                    Observation.Id,
  requirements:             UndoSetter[ScienceRequirements],
  pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
  scienceTargetIds:         SortedSet[Target.Id],
  baseCoordinates:          Option[Coordinates],
  obsConf:                  ObsConfiguration,
  selectedConfig:           View[ConfigSelection],
  revertedInstrumentConfig: List[ItcInstrumentConfig], // configuration rows selected if reverted
  modes:                    ScienceModes,
  customSedTimestamps:      List[Timestamp],
  allTargets:               TargetList,
  observingModeGroups:      ObservingModeGroupList,
  sequenceChanged:          Callback,
  readonly:                 Boolean,
  obsIdSetEditInfo:         ObsIdSetEditInfo,          // for the Position Angle Editor
  units:                    WavelengthUnits,
  isStaffOrAdmin:           Boolean,
  targetView:               View[Option[ItcTarget]]
) extends Tile[ConfigurationTile](
      ObsTabTileIds.ConfigurationId.id,
      "Configuration",
      bodyClass = ExploreStyles.ConfigurationTileBody
    )(ConfigurationTile):
  val observingMode: Option[ObservingMode]     = pacAndMode.get._2
  val mode: UndoSetter[Option[ObservingMode]]  =
    pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode)
  val posAngle: UndoSetter[PosAngleConstraint] =
    pacAndMode.zoom(PosAngleConstraintAndObsMode.posAngleConstraint)
  val obsIsReadonly: Boolean                   =
    readonly || obsIdSetEditInfo.hasExecuted
  // staff can edit acquisition settings for ongoing spectroscopy observations
  val acquisitionIsReadonly: Boolean           =
    readonly || (obsIdSetEditInfo.hasExecuted && !isStaffOrAdmin) || obsIdSetEditInfo.hasCompleted
  val selectorIsReadOnly: Boolean              =
    readonly || obsIdSetEditInfo.hasExecuted // even staff can't choose a new config if it is executed
  val itcTargets: EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
    scienceTargetIds.toItcTargets(allTargets)

object ConfigurationTile
    extends TileComponent[ConfigurationTile]({ (props, _) =>
      def pacAndModeAction(
        obsId:  Observation.Id
      )(using
        odbApi: OdbObservationApi[IO]
      ): Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode] =
        Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode](
          getter = identity,
          setter = x => _ => x
        )(
          onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
          onRestore = (_, pm) =>
            val (pac, oMode) = pm

            odbApi.updateObservations(
              List(obsId),
              ObservationPropertiesInput(
                observingMode = oMode.map(_.toInput).orUnassign,
                posAngleConstraint = pac.toInput.assign
              )
            )
        )

      def modeAction(
        obsId:  Observation.Id
      )(using
        odbApi: OdbObservationApi[IO]
      ): Action[Option[ObservingMode], Option[ObservingMode]] =
        Action[Option[ObservingMode], Option[ObservingMode]](
          getter = identity,
          setter = x => _ => x
        )(
          onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
          onRestore = (_, oMode) =>
            odbApi.updateObservations(
              List(obsId),
              ObservationPropertiesInput(observingMode = oMode.map(_.toInput).orUnassign)
            )
        )

      def updateConfiguration(
        obsId:                    Observation.Id,
        pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
        input:                    ObservingModeInput,
        defaultPosAngleConstrait: PosAngleOptions
      )(using odbApi: OdbObservationApi[IO]): IO[Unit] =
        val currentPac = pacAndMode.get._1
        if (defaultPosAngleConstrait != currentPac.toPosAngleOptions)
          val angle  =
            PosAngleConstraint.angle.getOption(currentPac).getOrElse(Angle.Angle0)
          val newPac = defaultPosAngleConstrait.toPosAngle(angle)
          odbApi
            .updateConfiguration(obsId, input.assign, newPac.toInput.assign)
            .flatMap: om =>
              pacAndModeAction(obsId)
                .set(pacAndMode)((newPac, om))
                .toAsync
        else
          odbApi
            .updateConfiguration(obsId, input.assign)
            .flatMap: om =>
              modeAction(obsId)
                .set(pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode))(om)
                .toAsync

      def revertConfiguration(
        obsId:                    Observation.Id,
        mode:                     UndoSetter[Option[ObservingMode]],
        revertedInstrumentConfig: List[ItcInstrumentConfig],
        selectedConfig:           View[ConfigSelection]
      )(using odbApi: OdbObservationApi[IO]): IO[Unit] =
        odbApi
          .updateConfiguration(obsId, Input.unassign) >>
          (modeAction(obsId).set(mode)(none) >>
            selectedConfig.set(
              ConfigSelection.fromInstrumentConfigs(revertedInstrumentConfig)
            )).toAsync

      /**
       * Handles the case where `A.Input[B]` not have an assigned value, but it needs to be created
       * for the `mod` function to work on.
       */
      def modInput[A, B](mod: (B => B) => A => A): (B => B) => Input[A] => Input[A] =
        f => inputA => inputA.map(a => mod(f)(a))

      val EmptyGmosNorthLongSlitInput: ObservingModeInput =
        ObservingModeInput.GmosNorthLongSlit(GmosNorthLongSlitInput())
      val EmptyGmosSouthLongSlitInput: ObservingModeInput =
        ObservingModeInput.GmosSouthLongSlit(GmosSouthLongSlitInput())
      val EmptyF2LongSlitInput: ObservingModeInput        =
        ObservingModeInput.Flamingos2LongSlit(Flamingos2LongSlitInput())
      val EmptyGmosNorthImagingInput: ObservingModeInput  =
        ObservingModeInput.GmosNorthImaging(GmosNorthImagingInput())
      val EmptyGmosSouthImagingInput: ObservingModeInput  =
        ObservingModeInput.GmosSouthImaging(GmosSouthImagingInput())

      for
        ctx        <- useContext(AppContext.ctx)
        isChanging <- useStateView(false)
      yield
        import ctx.given

        val revertConfig: Callback =
          revertConfiguration(
            props.obsId,
            props.mode,
            props.revertedInstrumentConfig,
            props.selectedConfig
          ).runAsyncAndForget

        val title =
          <.div(ExploreStyles.TileTitleConfigSelector)(
            DropdownOptional[ObservingModeSummary](
              value = props.observingMode.map(ObservingModeSummary.fromObservingMode),
              placeholder = "Choose existing observing mode...",
              disabled = isChanging.get,
              loading = isChanging.get,
              showClear = true,
              onChange = (om: Option[ObservingModeSummary]) =>
                om.fold(revertConfig)(m =>
                  updateConfiguration(
                    props.obsId,
                    props.pacAndMode,
                    m.toInput,
                    m.obsModeType.defaultPosAngleOptions
                  ).switching(isChanging.async).runAsync
                ),
              options = props.observingModeGroups.values.toList.sorted
                .map:
                  _.map: om =>
                    new SelectItem[ObservingModeSummary](value = om, label = om.shortName)
                .flattenOption
            ).unless(props.selectorIsReadOnly)
          )

        val posAngleConstraintAligner: Aligner[PosAngleConstraint, Input[PosAngleConstraintInput]] =
          Aligner(
            props.posAngle,
            UpdateObservationsInput(
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput()
            ),
            ctx.odbApi.updateObservations(_)
          ).zoom(
            Iso.id,
            UpdateObservationsInput.SET
              .andThen(ObservationPropertiesInput.posAngleConstraint)
              .modify
          )

        val posAngleConstraintView: View[PosAngleConstraint] =
          posAngleConstraintAligner.view(_.toInput.assign)

        def optModeAligner(
          input: ObservingModeInput
        ): Option[Aligner[ObservingMode, Input[ObservingModeInput]]] =
          Aligner(
            props.mode,
            UpdateObservationsInput(
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(observingMode = input.assign)
            ),
            (ctx.odbApi.updateObservations(_)).andThen(_.void)
          ).zoom( // Can we avoid the zoom and make an Aligner constructor that takes an input value?
            Iso.id,
            UpdateObservationsInput.SET.andThen(ObservationPropertiesInput.observingMode).modify
          ).toOption

        val optGmosNorthAligner: Option[Aligner[GmosNorthLongSlit, GmosNorthLongSlitInput]] =
          optModeAligner(EmptyGmosNorthLongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosNorthLongSlit,
              modInput:
                ObservingModeInput.gmosNorthLongSlit
                  .andThen(ObservingModeInput.GmosNorthLongSlit.value)
                  .modify
            )

        val optGmosSouthAligner: Option[Aligner[GmosSouthLongSlit, GmosSouthLongSlitInput]] =
          optModeAligner(EmptyGmosSouthLongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosSouthLongSlit,
              modInput:
                ObservingModeInput.gmosSouthLongSlit
                  .andThen(ObservingModeInput.GmosSouthLongSlit.value)
                  .modify
            )

        val optFlamingos2Aligner: Option[Aligner[Flamingos2LongSlit, Flamingos2LongSlitInput]] =
          optModeAligner(EmptyF2LongSlitInput).flatMap:
            _.zoomOpt(
              ObservingMode.flamingos2LongSlit,
              modInput:
                ObservingModeInput.flamingos2LongSlit
                  .andThen(ObservingModeInput.Flamingos2LongSlit.value)
                  .modify
            )

        val optGmosNorthImagingAligner: Option[Aligner[GmosNorthImaging, GmosNorthImagingInput]] =
          optModeAligner(EmptyGmosNorthImagingInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosNorthImaging,
              modInput:
                ObservingModeInput.gmosNorthImaging
                  .andThen(ObservingModeInput.GmosNorthImaging.value)
                  .modify
            )

        val optGmosSouthImagingAligner: Option[Aligner[GmosSouthImaging, GmosSouthImagingInput]] =
          optModeAligner(EmptyGmosSouthImagingInput).flatMap:
            _.zoomOpt(
              ObservingMode.gmosSouthImaging,
              modInput:
                ObservingModeInput.gmosSouthImaging
                  .andThen(ObservingModeInput.GmosSouthImaging.value)
                  .modify
            )

        val requirementsViewSet: ScienceRequirementsUndoView =
          ScienceRequirementsUndoView(props.obsId, props.requirements)

        val requirementsView: View[ScienceRequirements] =
          requirementsViewSet(
            Iso.id.asLens,
            UpdateScienceRequirements.scienceRequirements
          )

        val reqsExposureTimeMode: Option[ExposureTimeMode] =
          props.requirements.get.exposureTimeMode

        val body =
          React.Fragment(
            <.div(ExploreStyles.ConfigurationGrid)(
              props.obsConf.agsState
                .map(agsState =>
                  PAConfigurationPanel(
                    props.programId,
                    props.obsId,
                    posAngleConstraintView,
                    props.obsConf.selectedPA,
                    props.obsConf.averagePA,
                    agsState,
                    props.readonly, // readonly status is more complicated here...
                    props.obsIdSetEditInfo,
                    props.isStaffOrAdmin
                  )
                ),
              if (props.mode.get.isEmpty)
                props.obsConf.constraints
                  .map(constraints =>
                    BasicConfigurationPanel(
                      props.userId,
                      props.obsId,
                      requirementsView,
                      props.selectedConfig,
                      constraints,
                      props.itcTargets,
                      props.baseCoordinates,
                      props.obsConf.calibrationRole,
                      props.selectedConfig.get
                        .toBasicConfiguration()
                        .map: bc =>
                          updateConfiguration(
                            props.obsId,
                            props.pacAndMode,
                            bc.toInput,
                            bc.obsModeType.defaultPosAngleOptions
                          )
                        .orEmpty,
                      props.modes,
                      props.customSedTimestamps,
                      props.obsIsReadonly,
                      props.units,
                      props.targetView
                    )
                  )
              else
                React.Fragment(
                  // Gmos North Long Slit
                  optGmosNorthAligner.map: northAligner =>
                    GmosLongslitConfigPanel
                      .GmosNorthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        northAligner,
                        revertConfig,
                        props.modes.spectroscopy,
                        props.sequenceChanged,
                        props.obsIsReadonly,
                        props.acquisitionIsReadonly,
                        props.units
                      ),
                  // Gmos South Long Slit
                  optGmosSouthAligner.map: southAligner =>
                    GmosLongslitConfigPanel
                      .GmosSouthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        southAligner,
                        revertConfig,
                        props.modes.spectroscopy,
                        props.sequenceChanged,
                        props.obsIsReadonly,
                        props.acquisitionIsReadonly,
                        props.units
                      ),
                  // Gmos North Imaging
                  optGmosNorthImagingAligner.map: aligner =>
                    GmosImagingConfigPanel.GmosNorthImaging(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      aligner,
                      reqsExposureTimeMode,
                      revertConfig,
                      props.sequenceChanged,
                      props.obsIsReadonly,
                      props.units
                    ),
                  // Gmos South Imaging
                  optGmosSouthImagingAligner.map: aligner =>
                    GmosImagingConfigPanel.GmosSouthImaging(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      aligner,
                      reqsExposureTimeMode,
                      revertConfig,
                      props.sequenceChanged,
                      props.obsIsReadonly,
                      props.units
                    ),
                  // Flamingos2 Long Slit
                  optFlamingos2Aligner.map: f2Aligner =>
                    Flamingos2LongslitConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      f2Aligner,
                      revertConfig,
                      props.modes.spectroscopy,
                      props.sequenceChanged,
                      props.obsIsReadonly,
                      props.acquisitionIsReadonly,
                      props.units,
                      props.isStaffOrAdmin
                    )
                )
            )
          )

        TileContents(title, body)
    })
