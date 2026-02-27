// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.ExploreModelValidators
import explore.model.enums.WavelengthUnits
import explore.modes.ModeCommonWavelengths
import explore.modes.ModeWavelength
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import japgolly.scalajs.react.*
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.optics.Format
import lucuma.core.syntax.string.parseBigDecimalOption
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.schemas.model.ObservingMode
import lucuma.ui.input.ChangeAuditor

trait ConfigurationFormats:
  private lazy val angleArcsecondsBaseAuditor = ChangeAuditor
    .fromInputValidWedge(ExploreModelValidators.decimalArcsecondsValidWedge)
    .allow(s => s === "0" || s === "0.")

  lazy val angleArcsecondsChangeAuditor = angleArcsecondsBaseAuditor
    .decimal(2.refined)
    .optional

  lazy val angleArcsecsFormat   = ExploreModelValidators.decimalArcsecondsValidWedge.optional
  lazy val wvMicroInput         = ExploreModelValidators.wavelengthMicroValidWedge.optional
  lazy val wvNanoInput          = ExploreModelValidators.wavelengthNanoValidWedge.optional
  lazy val wvAngstromInput      = ExploreModelValidators.wavelengthAngstromValidWedge.optional
  lazy val wvDeltaMicroInput    = ExploreModelValidators.wavelengthMicroDeltaValidWedge.optional
  lazy val wvDeltaNanoInput     = ExploreModelValidators.wavelengthNanoDeltaValidWedge.optional
  lazy val wvDeltaAngstromInput = ExploreModelValidators.wavelengthAngstromDeltaValidWedge.optional

  private def bdFormat(scale: Int): Format[String, BigDecimal] =
    Format[String, BigDecimal](parseBigDecimalOption, _.setScale(scale).toString)

  extension (u: WavelengthUnits)
    def toAuditor: ChangeAuditor =
      u match
        case WavelengthUnits.Micrometers => ChangeAuditor.posBigDecimal(3.refined)
        case WavelengthUnits.Nanometers  => ChangeAuditor.posBigDecimal(1.refined)
        case WavelengthUnits.Angstroms   => ChangeAuditor.posInt

    def toSNAuditor: ChangeAuditor = toAuditor

    def toInputWedge: InputValidWedge[Option[Wavelength]] =
      u match
        case WavelengthUnits.Micrometers => wvMicroInput
        case WavelengthUnits.Nanometers  => wvNanoInput
        case WavelengthUnits.Angstroms   => wvAngstromInput

    def toInputFormat: InputValidFormat[Wavelength] =
      u match
        case WavelengthUnits.Micrometers => ExploreModelValidators.wavelengthMicroValidWedge
        case WavelengthUnits.Nanometers  => ExploreModelValidators.wavelengthNanoValidWedge
        case WavelengthUnits.Angstroms   => ExploreModelValidators.wavelengthAngstromValidWedge

    def format: Format[String, Wavelength] =
      u match
        case WavelengthUnits.Micrometers => bdFormat(2).andThen(Wavelength.decimalMicrometers)
        case WavelengthUnits.Nanometers  => bdFormat(1).andThen(Wavelength.decimalNanometers)
        case WavelengthUnits.Angstroms   => bdFormat(0).andThen(Wavelength.decimalAngstroms)

    def toDeltaInputWedge: InputValidWedge[Option[WavelengthDelta]] =
      u match
        case WavelengthUnits.Micrometers => wvDeltaMicroInput
        case WavelengthUnits.Nanometers  => wvDeltaNanoInput
        case WavelengthUnits.Angstroms   => wvDeltaAngstromInput

object ConfigurationFormats extends ConfigurationFormats

case class ModeData private (
  resolution: PosInt,
  λmin:       ModeWavelength,
  λmax:       ModeWavelength,
  λdelta:     WavelengthDelta
) extends ModeCommonWavelengths

object ModeData:
  def fromSpectroscopyModeRow(row: SpectroscopyModeRow): ModeData =
    ModeData(row.resolution, row.λmin, row.λmax, row.λdelta)

def useModeData(
  confMatrix: SpectroscopyModesMatrix,
  obsMode:    ObservingMode
): HookResult[Reusable[Option[ModeData]]] = {
  // a reusablity based only on what is used here
  given Reusability[ObservingMode] = Reusability:
    case (ObservingMode.GmosNorthLongSlit(grating = xGrating, filter = xFilter, fpu = xFpu),
          ObservingMode.GmosNorthLongSlit(grating = yGrating, filter = yFilter, fpu = yFpu)
        ) =>
      xGrating === yGrating && xFilter === yFilter && xFpu === yFpu
    case (ObservingMode.GmosSouthLongSlit(grating = xGrating, filter = xFilter, fpu = xFpu),
          ObservingMode.GmosSouthLongSlit(grating = yGrating, filter = yFilter, fpu = yFpu)
        ) =>
      xGrating === yGrating && xFilter === yFilter && xFpu === yFpu
    case (ObservingMode.Flamingos2LongSlit(disperser = xDisperser, filter = xFilter, fpu = xFpu),
          ObservingMode.Flamingos2LongSlit(disperser = yDisperser, filter = yFilter, fpu = yFpu)
        ) =>
      xDisperser === yDisperser && xFilter === yFilter && xFpu === yFpu
    case _ => false

  for row <- useMemo((obsMode, confMatrix.matrix.length)): (obsMode, _) =>
               confMatrix.getRowByInstrumentConfig(obsMode)
  yield row.map(_.map(ModeData.fromSpectroscopyModeRow(_)))
}

enum ConfigEditPermissions derives Eq:
  case Readonly, OnlyForOngoing, FullEdit

  def fold[A](ro: => A, ongoing: => A, full: => A): A = this match
    case Readonly       => ro
    case OnlyForOngoing => ongoing
    case FullEdit       => full

  def isReadonly: Boolean       = fold(true, false, false)
  def isOnlyForOngoing: Boolean = fold(false, true, false)
  def isFullEdit: Boolean       = fold(false, false, true)
