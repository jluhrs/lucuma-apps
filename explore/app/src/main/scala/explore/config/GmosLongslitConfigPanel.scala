// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import coulomb.Quantity
import crystal.react.View
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.ModeWavelength
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens
import org.typelevel.log4cats.Logger

object GmosLongslitConfigPanel {
  sealed trait GmosLongslitConfigPanel[T <: ObservingMode, Input] {
    def programId: Program.Id
    def obsId: Observation.Id
    def calibrationRole: Option[CalibrationRole]
    def observingMode: Aligner[T, Input]
    def revertConfig: Callback
    def confMatrix: SpectroscopyModesMatrix
    def sequenceChanged: Callback
    def permissions: ConfigEditPermissions
    def units: WavelengthUnits
    def instrument = observingMode.get.instrument
  }

  sealed abstract class GmosLongslitConfigPanelBuilder[
    T <: ObservingMode,
    Input,
    Props <: GmosLongslitConfigPanel[T, Input],
    Grating: Enumerated: Display,
    Filter: Enumerated: Display,
    Fpu: Enumerated: Display
  ] {
    protected type AA = Aligner[T, Input]

    inline protected def isCustomized(aligner: AA): Boolean = aligner.get.isCustomized

    protected def revertCustomizations(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Callback

    protected def centralWavelength(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength]

    protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Grating]

    protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Filter]]

    protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Fpu]

    protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]]

    protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]]

    protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]]

    protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]]

    protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[
      Option[NonEmptyList[WavelengthDither]]
    ]

    protected def explicitOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]]

    protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode]

    protected def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Filter]]

    protected def explicitAcquisitionRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosLongSlitAcquisitionRoi]]

    protected def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode]

    protected val initialGratingLens: Lens[T, Grating]
    protected val initialFilterLens: Lens[T, Option[Filter]]
    protected val initialFpuLens: Lens[T, Fpu]
    protected val initialCentralWavelengthLens: Lens[T, Wavelength]
    protected val defaultXBinningLens: Lens[T, GmosXBinning]
    protected val defaultYBinningLens: Lens[T, GmosYBinning]
    protected val defaultReadModeGainLens: Lens[T, (GmosAmpReadMode, GmosAmpGain)]
    protected val defaultRoiLens: Lens[T, GmosRoi]
    protected val defaultWavelengthDithersLens: Lens[T, NonEmptyList[WavelengthDither]]
    protected val defaultOffsetsLens: Lens[T, NonEmptyList[Offset.Q]]

    protected val excludedAcquisitionFilters: Set[Filter]
    protected val defaultAcquisitionFilterLens: Lens[T, Filter]
    protected val defaultAcquisitionRoiLens: Lens[T, GmosLongSlitAcquisitionRoi]

    protected def resolvedReadModeGainGetter: T => (GmosAmpReadMode, GmosAmpGain)

    protected given Display[(GmosAmpReadMode, GmosAmpGain)] =
      Display.by( // Shortname is in lower case for some reason
        { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
        { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
      )

    val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx       <- useContext(AppContext.ctx)
          modeData  <- useModeData(props.confMatrix, props.observingMode.get)
          editState <- useStateView(ConfigEditState.View)
        yield
          import ctx.given

          val disableAdvancedEdit      =
            editState.get =!= ConfigEditState.AdvancedEdit || !props.permissions.isFullEdit
          val disableSimpleEdit        =
            disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
          val disableAdvancedAcqEdit   = disableAdvancedEdit && !props.permissions.isOnlyForOngoing
          val showCustomization        = props.calibrationRole.isEmpty
          val allowRevertCustomization = props.permissions.isFullEdit

          val centralWavelengthView    = centralWavelength(props.observingMode)
          val initialCentralWavelength = initialCentralWavelengthLens.get(props.observingMode.get)

          val defaultXBinning          = defaultXBinningLens.get(props.observingMode.get)
          val defaultYBinning          = defaultYBinningLens.get(props.observingMode.get)
          val defaultReadModeGain      = defaultReadModeGainLens.get(props.observingMode.get)
          val defaultRoi               = defaultRoiLens.get(props.observingMode.get)
          val resolvedReadModeGain     = resolvedReadModeGainGetter(props.observingMode.get)
          val defaultAcquisitionFilter = defaultAcquisitionFilterLens.get(props.observingMode.get)
          val defaultAcquisitionRoi    = defaultAcquisitionRoiLens.get(props.observingMode.get)

          val validDithers = modeData.value
            .map: mode =>
              ExploreModelValidators.dithersValidWedge(
                centralWavelengthView.get,
                mode.λmin.value,
                mode.λmax.value
              )
            .getOrElse(ExploreModelValidators.ditherValidWedge)
            .toNel(",".refined)
            .withErrorMessage(_ => "Invalid wavelength dither values".refined)
            .optional

          def dithersControl(onChange: Callback): VdomElement =
            val default = defaultWavelengthDithersLens.get(props.observingMode.get)
            val view    = explicitWavelengthDithers(props.observingMode)
            CustomizableInputTextOptional(
              id = "dithers".refined,
              value = view.withOnMod(_ => onChange),
              defaultValue = default,
              label = React.Fragment("λ Dithers",
                                     HelpIcon("configuration/gmos/lambda-dithers.md".refined)
              ),
              validFormat = validDithers,
              changeAuditor = ChangeAuditor
                .bigDecimal(integers = 3.refined, decimals = 1.refined)
                .toSequence()
                .optional,
              units = "nm".some,
              disabled = disableSimpleEdit,
              showCustomization = showCustomization,
              allowRevertCustomization = allowRevertCustomization
            )

          React.Fragment(
            <.div(ExploreStyles.GmosLongSlitUpperGrid)(
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                CustomizableEnumSelect(
                  id = "grating".refined,
                  view = grating(props.observingMode),
                  defaultValue = initialGratingLens.get(props.observingMode.get),
                  label = "Grating".some,
                  helpId = Some("configuration/gmos/grating.md".refined),
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                CustomizableEnumSelectOptional(
                  id = "filter".refined,
                  view = filter(props.observingMode),
                  defaultValue = initialFilterLens.get(props.observingMode.get),
                  label = "Filter".some,
                  helpId = Some("configuration/gmos/filter.md".refined),
                  disabled = disableAdvancedEdit,
                  showClear = true,
                  resetToOriginal = true,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                CustomizableEnumSelect(
                  id = "fpu".refined,
                  view = fpu(props.observingMode),
                  defaultValue = initialFpuLens.get(props.observingMode.get),
                  label = "FPU".some,
                  helpId = Some("configuration/gmos/fpu.md".refined),
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                OffsetsControl(
                  explicitOffsets(props.observingMode),
                  defaultOffsetsLens.get(props.observingMode.get),
                  props.sequenceChanged,
                  disableSimpleEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              ),
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                CustomizableInputText(
                  id = "central-wavelength".refined,
                  value = centralWavelengthView,
                  label =
                    React.Fragment("Central Wavelength",
                                   HelpIcon("configuration/gmos/central=wavelength.md".refined)
                    ),
                  units = props.units.symbol.some,
                  validFormat = props.units.toInputFormat,
                  changeAuditor = props.units.toAuditor,
                  defaultValue = initialCentralWavelength,
                  disabled = disableSimpleEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                dithersControl(props.sequenceChanged),
                ExposureTimeModeEditor(
                  props.instrument.some,
                  none,
                  exposureTimeMode(props.observingMode),
                  ScienceMode.Spectroscopy,
                  !props.permissions.isFullEdit,
                  props.units,
                  props.calibrationRole,
                  "gmosLongslit".refined
                )
              ),
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                // Provide better accessibility by using aria-label directly
                // on the dropdowns so X and Y binning are correctly labeled.
                <.label(
                  ^.htmlFor := "explicitXBin",
                  LucumaPrimeStyles.FormFieldLabel,
                  "Binning",
                  HelpIcon("configuration/gmos/binning.md".refined)
                ),
                <.div(
                  ExploreStyles.GmosLongSlitBinning,
                  CustomizableEnumSelectOptional(
                    id = "explicitXBin".refined,
                    view = explicitXBinning(props.observingMode).withDefault(defaultXBinning),
                    defaultValue = defaultXBinning.some,
                    disabled = disableAdvancedEdit,
                    dropdownMods = ^.aria.label := "X Binning",
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  ),
                  <.label(^.htmlFor := "explicitYBin", "x"),
                  CustomizableEnumSelectOptional(
                    id = "explicitYBin".refined,
                    view = explicitYBinning(props.observingMode).withDefault(defaultYBinning),
                    defaultValue = defaultYBinning.some,
                    disabled = disableAdvancedEdit,
                    dropdownMods = ^.aria.label := "Y Binning",
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  )
                ),
                CustomizableEnumSelectOptional(
                  id = "explicitReadMode".refined,
                  view = explicitReadModeGain(props.observingMode)
                    .withDefault(defaultReadModeGain, resolvedReadModeGain),
                  defaultValue = defaultReadModeGain.some,
                  label = "Read Mode".some,
                  helpId = Some("configuration/gmos/read-mode.md".refined),
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                CustomizableEnumSelectOptional(
                  id = "explicitRoi".refined,
                  view = explicitRoi(props.observingMode).withDefault(defaultRoi),
                  defaultValue = defaultRoi.some,
                  label = "ROI".some,
                  helpId = Some("configuration/gmos/roi.md".refined),
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                LambdaAndIntervalFormValues(
                  modeData = modeData,
                  centralWavelength = centralWavelengthView.get,
                  units = props.units
                )
              )
            ),
            <.div(
              ExploreStyles.GmosLongSlitLowerGrid,
              Panel(
                header = <.span(
                  "Acquisition",
                  HelpIcon("configuration/gmos/acquisition-customization.md".refined)
                ),
                toggleable = true,
                collapsed = true
              )(
                <.div(
                  ExploreStyles.AcquisitionCustomizationGrid,
                  <.div(
                    LucumaPrimeStyles.FormColumnCompact,
                    CustomizableEnumSelectOptional(
                      id = "acq-explicit-roi".refined,
                      view = explicitAcquisitionRoi(props.observingMode)
                        .withDefault(defaultAcquisitionRoi),
                      defaultValue = defaultAcquisitionRoi.some,
                      label = "ROI".some,
                      helpId = None,
                      disabled = disableAdvancedAcqEdit,
                      showCustomization = showCustomization,
                      allowRevertCustomization =
                        allowRevertCustomization || props.permissions.isOnlyForOngoing
                    ),
                    CustomizableEnumSelectOptional(
                      id = "acq-explicit-filter".refined,
                      view = explicitAcquisitionFilter(props.observingMode)
                        .withDefault(defaultAcquisitionFilter),
                      defaultValue = defaultAcquisitionFilter.some,
                      exclude = excludedAcquisitionFilters,
                      label = "Filter".some,
                      helpId = None,
                      disabled = disableAdvancedAcqEdit,
                      showCustomization = showCustomization,
                      allowRevertCustomization =
                        allowRevertCustomization || props.permissions.isOnlyForOngoing
                    )
                  ),
                  <.div(
                    LucumaPrimeStyles.FormColumnCompact,
                    ExposureTimeModeEditor(
                      props.observingMode.get.instrument.some,
                      none,
                      acquisitionExposureTimeModeView(props.observingMode),
                      ScienceMode.Imaging,
                      props.permissions.isReadonly,
                      props.units,
                      props.calibrationRole,
                      "gmosAcq".refined,
                      forceCount = Some(1.refined)
                    )
                  )
                )
              ),
              AdvancedConfigButtons(
                editState = editState,
                isCustomized = isCustomized(props.observingMode),
                revertConfig = props.revertConfig,
                revertCustomizations = revertCustomizations(props.observingMode),
                sequenceChanged = props.sequenceChanged,
                readonly = !props.permissions.isFullEdit
              )
            )
          )
  }

  // Gmos North Long Slit
  case class GmosNorthLongSlit(
    programId:       Program.Id,
    obsId:           Observation.Id,
    calibrationRole: Option[CalibrationRole],
    observingMode:   Aligner[ObservingMode.GmosNorthLongSlit, GmosNorthLongSlitInput],
    revertConfig:    Callback,
    confMatrix:      SpectroscopyModesMatrix,
    sequenceChanged: Callback,
    permissions:     ConfigEditPermissions,
    units:           WavelengthUnits
  ) extends ReactFnProps[GmosLongslitConfigPanel.GmosNorthLongSlit](
        GmosLongslitConfigPanel.GmosNorthLongSlit.component
      )
      with GmosLongslitConfigPanel[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput
      ]

  object GmosNorthLongSlit
      extends GmosLongslitConfigPanelBuilder[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput,
        GmosLongslitConfigPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu
      ] {

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override protected def centralWavelength(
      aligner: AA
    )(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
          GmosNorthLongSlitInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.grating,
          GmosNorthLongSlitInput.grating.modify
        )
        .view(_.assign)

    inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.filter,
        GmosNorthLongSlitInput.filter.modify
      )
      .view(_.orUnassign)

    inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthFpu] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.fpu,
        GmosNorthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitXBin,
        GmosNorthLongSlitInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

    inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitYBin,
        GmosNorthLongSlitInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosNorthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosNorthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthLongSlitInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosNorthLongSlitInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitRoi,
        GmosNorthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitWavelengthDithers,
        GmosNorthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

    inline override protected def explicitOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitOffsets,
        GmosNorthLongSlitInput.explicitOffsets.modify
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    inline protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.exposureTimeMode,
        GmosNorthLongSlitInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    inline private def acquisition(
      aligner: AA
    ): Aligner[ObservingMode.GmosNorthLongSlit.Acquisition, GmosNorthLongSlitAcquisitionInput] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.acquisition,
          forceAssign(GmosNorthLongSlitInput.acquisition.modify)(
            GmosNorthLongSlitAcquisitionInput()
          )
        )

    inline override protected def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthLongSlit.Acquisition.explicitFilter,
            GmosNorthLongSlitAcquisitionInput.explicitFilter.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitAcquisitionRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosLongSlitAcquisitionRoi]] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthLongSlit.Acquisition.explicitRoi,
            GmosNorthLongSlitAcquisitionInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthLongSlit.Acquisition.exposureTimeMode,
            GmosNorthLongSlitAcquisitionInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    override protected val initialGratingLens           =
      ObservingMode.GmosNorthLongSlit.initialGrating
    override protected val initialFilterLens            = ObservingMode.GmosNorthLongSlit.initialFilter
    override protected val initialFpuLens               = ObservingMode.GmosNorthLongSlit.initialFpu
    override protected val initialCentralWavelengthLens =
      ObservingMode.GmosNorthLongSlit.initialCentralWavelength.andThen(CentralWavelength.Value)
    protected val defaultBinningLens                    =
      (ObservingMode.GmosNorthLongSlit.defaultXBin,
       ObservingMode.GmosNorthLongSlit.defaultYBin
      ).disjointZip
    protected val defaultReadModeGainLens               =
      (ObservingMode.GmosNorthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosNorthLongSlit.defaultAmpGain
      ).disjointZip
    protected val defaultXBinningLens                   = ObservingMode.GmosNorthLongSlit.defaultXBin
    protected val defaultYBinningLens                   = ObservingMode.GmosNorthLongSlit.defaultYBin
    protected val defaultRoiLens                        = ObservingMode.GmosNorthLongSlit.defaultRoi
    override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosNorthLongSlit.defaultWavelengthDithers
    override protected val defaultOffsetsLens           =
      ObservingMode.GmosNorthLongSlit.defaultOffsets

    override protected val excludedAcquisitionFilters: Set[GmosNorthFilter] =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet

    override protected val defaultAcquisitionFilterLens
      : Lens[ObservingMode.GmosNorthLongSlit, GmosNorthFilter] =
      ObservingMode.GmosNorthLongSlit.acquisition.andThen(
        ObservingMode.GmosNorthLongSlit.Acquisition.defaultFilter
      )
    override protected val defaultAcquisitionRoiLens
      : Lens[ObservingMode.GmosNorthLongSlit, GmosLongSlitAcquisitionRoi] =
      ObservingMode.GmosNorthLongSlit.acquisition.andThen(
        ObservingMode.GmosNorthLongSlit.Acquisition.defaultRoi
      )

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosNorthLongSlit.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosNorthLongSlit.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }

// Gmos South Long Slit

  case class GmosSouthLongSlit(
    programId:       Program.Id,
    obsId:           Observation.Id,
    calibrationRole: Option[CalibrationRole],
    observingMode:   Aligner[ObservingMode.GmosSouthLongSlit, GmosSouthLongSlitInput],
    revertConfig:    Callback,
    confMatrix:      SpectroscopyModesMatrix,
    sequenceChanged: Callback,
    permissions:     ConfigEditPermissions,
    units:           WavelengthUnits
  ) extends ReactFnProps[GmosLongslitConfigPanel.GmosSouthLongSlit](
        GmosLongslitConfigPanel.GmosSouthLongSlit.component
      )
      with GmosLongslitConfigPanel[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput
      ]

  object GmosSouthLongSlit
      extends GmosLongslitConfigPanelBuilder[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput,
        GmosLongslitConfigPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu
      ] {

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
          GmosSouthLongSlitInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.grating,
          GmosSouthLongSlitInput.grating.modify
        )
        .view(_.assign)

    inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.filter,
          GmosSouthLongSlitInput.filter.modify
        )
        .view(_.orUnassign)

    inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthFpu] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.fpu,
        GmosSouthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitXBin,
        GmosSouthLongSlitInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

    inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitYBin,
        GmosSouthLongSlitInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosSouthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosSouthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthLongSlitInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosSouthLongSlitInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitRoi,
        GmosSouthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitWavelengthDithers,
        GmosSouthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(
        _.map(
          _.map(d => WavelengthDitherInput.Picometers(d.toPicometers.value)).toList
        ).orUnassign
      )

    inline override protected def explicitOffsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[Offset.Q]]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitOffsets,
        GmosSouthLongSlitInput.explicitOffsets.modify
      )
      .view(_.map(_.toList.map(_.toInput)).orUnassign)

    inline protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.exposureTimeMode,
        GmosSouthLongSlitInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    inline private def acquisition(
      aligner: AA
    ): Aligner[ObservingMode.GmosSouthLongSlit.Acquisition, GmosSouthLongSlitAcquisitionInput] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.acquisition,
          forceAssign(GmosSouthLongSlitInput.acquisition.modify)(
            GmosSouthLongSlitAcquisitionInput()
          )
        )

    override protected val excludedAcquisitionFilters: Set[GmosSouthFilter] =
      Enumerated[GmosSouthFilter].all.toSet -- GmosSouthFilter.acquisition.toList.toSet

    inline override protected def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthLongSlit.Acquisition.explicitFilter,
            GmosSouthLongSlitAcquisitionInput.explicitFilter.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitAcquisitionRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosLongSlitAcquisitionRoi]] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthLongSlit.Acquisition.explicitRoi,
            GmosSouthLongSlitAcquisitionInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthLongSlit.Acquisition.exposureTimeMode,
            GmosSouthLongSlitAcquisitionInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    override protected val initialGratingLens           =
      ObservingMode.GmosSouthLongSlit.initialGrating
    override protected val initialFilterLens            = ObservingMode.GmosSouthLongSlit.initialFilter
    override protected val initialFpuLens               = ObservingMode.GmosSouthLongSlit.initialFpu
    override protected val initialCentralWavelengthLens =
      ObservingMode.GmosSouthLongSlit.initialCentralWavelength.andThen(CentralWavelength.Value)
    protected val defaultBinningLens                    =
      (ObservingMode.GmosSouthLongSlit.defaultXBin,
       ObservingMode.GmosSouthLongSlit.defaultYBin
      ).disjointZip
    protected val defaultXBinningLens                   = ObservingMode.GmosSouthLongSlit.defaultXBin
    protected val defaultYBinningLens                   = ObservingMode.GmosSouthLongSlit.defaultYBin
    protected val defaultReadModeGainLens               =
      (ObservingMode.GmosSouthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosSouthLongSlit.defaultAmpGain
      ).disjointZip
    protected val defaultRoiLens                        = ObservingMode.GmosSouthLongSlit.defaultRoi
    override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosSouthLongSlit.defaultWavelengthDithers
    override protected val defaultOffsetsLens           =
      ObservingMode.GmosSouthLongSlit.defaultOffsets

    override protected val defaultAcquisitionFilterLens
      : Lens[ObservingMode.GmosSouthLongSlit, GmosSouthFilter] =
      ObservingMode.GmosSouthLongSlit.acquisition.andThen(
        ObservingMode.GmosSouthLongSlit.Acquisition.defaultFilter
      )
    override protected val defaultAcquisitionRoiLens
      : Lens[ObservingMode.GmosSouthLongSlit, GmosLongSlitAcquisitionRoi] =
      ObservingMode.GmosSouthLongSlit.acquisition.andThen(
        ObservingMode.GmosSouthLongSlit.Acquisition.defaultRoi
      )

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosSouthLongSlit.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosSouthLongSlit.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }
}
