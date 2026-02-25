// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.OffsetInput
import explore.model.AppContext
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*

final case class Flamingos2LongslitConfigPanel(
  programId:           Program.Id,
  obsId:               Observation.Id,
  calibrationRole:     Option[CalibrationRole],
  observingMode:       Aligner[ObservingMode.Flamingos2LongSlit, Flamingos2LongSlitInput],
  revertConfig:        Callback,
  confMatrix:          SpectroscopyModesMatrix,
  sequenceChanged:     Callback,
  readonly:            Boolean,
  acquisitionReadonly: Boolean,
  units:               WavelengthUnits,
  isStaff:             Boolean
) extends ReactFnProps(Flamingos2LongslitConfigPanel)

object Flamingos2LongslitConfigPanel
    extends ReactFnComponent[Flamingos2LongslitConfigPanel](props =>
      for
        ctx                 <- useContext(AppContext.ctx)
        modeData            <-
          useModeData(props.confMatrix, props.observingMode.get)
        editState           <- useStateView(ConfigEditState.View)
        unModdedOffsetsView <- useStateView(props.observingMode.get.offsets)
        _                   <- useEffectWithDeps(props.observingMode.get.offsets):
                                 unModdedOffsetsView.set
      yield
        import ctx.given

        val disableAdvancedEdit      = editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
        val disableSimpleEdit        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = !props.readonly

        val disperserView: View[Flamingos2Disperser] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.disperser,
            Flamingos2LongSlitInput.disperser.modify
          )
          .view(_.assign)

        val filterView: View[Flamingos2Filter] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.filter,
            Flamingos2LongSlitInput.filter.modify
          )
          .view(_.assign)

        val fpuView: View[Flamingos2Fpu] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.fpu,
            Flamingos2LongSlitInput.fpu.modify
          )
          .view(_.assign)

        val readModeView: View[Option[Flamingos2ReadMode]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitReadMode,
            Flamingos2LongSlitInput.explicitReadMode.modify
          )
          .view(_.orUnassign)

        val explicitOffsetsView: View[Option[NonEmptyList[Offset]]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitOffsets,
            Flamingos2LongSlitInput.explicitOffsets.modify
          )
          .view(_.map(_.toList.map(_.toInput)).orUnassign)

        val defaultOffsets: NonEmptyList[Offset] = props.observingMode.get.defaultOffsets

        val localOffsetsView: View[NonEmptyList[Offset]] =
          unModdedOffsetsView.withOnMod: nel =>
            val newOffsets =
              if nel === defaultOffsets
              then none
              else nel.some
            explicitOffsetsView.set(newOffsets)

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.exposureTimeMode,
            Flamingos2LongSlitInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        given Enumerated[Option[Flamingos2ReadMode]] =
          deriveOptionalEnumerated[Flamingos2ReadMode]("Auto")
        given Display[Option[Flamingos2ReadMode]]    =
          deriveOptionalDisplay[Flamingos2ReadMode]("Auto")

        val deckerView: View[Option[Flamingos2Decker]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitDecker,
            Flamingos2LongSlitInput.explicitDecker.modify
          )
          .view(_.orUnassign)

        val acquisition: Aligner[ObservingMode.Flamingos2LongSlit.Acquisition,
                                 Flamingos2LongSlitAcquisitionInput
        ] =
          props.observingMode.zoom(
            ObservingMode.Flamingos2LongSlit.acquisition,
            forceAssign(Flamingos2LongSlitInput.acquisition.modify)(
              Flamingos2LongSlitAcquisitionInput()
            )
          )

        val acquisitionExposureTimeView: View[ExposureTimeMode] =
          acquisition
            .zoom(ObservingMode.Flamingos2LongSlit.Acquisition.exposureTimeMode,
                  Flamingos2LongSlitAcquisitionInput.exposureTimeMode.modify
            )
            .view(_.toInput.assign)

        val defaultDecker = props.observingMode.get.defaultDecker

        React.Fragment(
          <.div(
            ExploreStyles.Flamingos2UpperGrid
          )(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              CustomizableEnumSelect(
                id = "disperser".refined,
                view = disperserView,
                defaultValue = props.observingMode.get.initialDisperser,
                label = "Disperser".some,
                helpId = Some("configuration/f2/disperser.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "filter".refined,
                view = filterView,
                defaultValue = props.observingMode.get.initialFilter,
                label = "Filter".some,
                helpId = Some("configuration/f2/filter.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "fpu".refined,
                view = fpuView,
                defaultValue = props.observingMode.get.initialFpu,
                label = "FPU".some,
                helpId = Some("configuration/f2/fpu.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "read-mode".refined,
                view = readModeView,
                defaultValue = None,
                label = "Read Mode".some,
                helpId = Some("configuration/f2/read-mode.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              <.span(
                "Spatial Offsets",
                HelpIcon("configuration/f2/spatial-offsets.md".refined),
                CustomizedGroupAddon(
                  "original",
                  explicitOffsetsView.set(none),
                  allowRevertCustomization
                ).when(explicitOffsetsView.get.isDefined)
              ),
              React.Fragment(
                localOffsetsView.toNelOfViews.toList.zipWithIndex
                  .map: (offsetView, idx) =>
                    OffsetInput(
                      id = NonEmptyString.unsafeFrom(s"spatial-offsets-$idx"),
                      offset = offsetView,
                      readonly = disableSimpleEdit,
                      clazz = LucumaPrimeStyles.FormField
                    )
                  .toVdomArray
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ExposureTimeModeEditor(
                props.observingMode.get.instrument.some,
                none,
                exposureTimeMode,
                ScienceMode.Spectroscopy,
                props.readonly,
                props.units,
                props.calibrationRole,
                "f2LongSlit".refined
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormLabel(htmlFor = "decker".refined)("Decker",
                                                    HelpIcon("configuration/f2/decker.md".refined)
              ),
              if (props.isStaff)
                CustomizableEnumSelectOptional(
                  id = "decker".refined,
                  view = deckerView.withDefault(defaultDecker),
                  defaultValue = defaultDecker.some,
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              else
                <.label(^.id := "decker",
                        ExploreStyles.FormValue,
                        deckerView.get.getOrElse(defaultDecker).shortName
                ),
              // Per Andy, we'll use the wavelength of the filter as the central wavelength
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = filterView.get.wavelength,
                units = props.units
              )
            )
          ),
          <.div(
            ExploreStyles.Flamingos2LowerGrid,
            Panel(
              header = <.span("Acquisition",
                              HelpIcon("configuration/f2/acquisition-customization.md".refined)
              ),
              toggleable = true,
              collapsed = true
            )(
              <.div(
                ExploreStyles.AcquisitionCustomizationGrid,
                <.div(
                  LucumaPrimeStyles.FormColumnCompact,
                  ExposureTimeModeEditor(
                    props.observingMode.get.instrument.some,
                    none,
                    acquisitionExposureTimeView,
                    ScienceMode.Imaging,
                    props.acquisitionReadonly,
                    props.units,
                    props.calibrationRole,
                    "f2Acq".refined,
                    forceCount = Some(1.refined)
                  )
                )
              )
            ),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = props.readonly,
              showAdvancedButton = props.isStaff
            )
          )
        )
    )
