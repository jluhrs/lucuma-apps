// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.derived.*
import cats.kernel.Order
import clue.data.syntax.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ObservingModeType
import lucuma.core.util.Display
import lucuma.schemas.ObservationDB.Types.Flamingos2LongSlitInput
import lucuma.schemas.ObservationDB.Types.GmosNorthImagingInput
import lucuma.schemas.ObservationDB.Types.GmosNorthLongSlitInput
import lucuma.schemas.ObservationDB.Types.GmosSouthImagingInput
import lucuma.schemas.ObservationDB.Types.GmosSouthLongSlitInput
import lucuma.schemas.ObservationDB.Types.ObservingModeInput
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.GmosImagingVariant
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*

// Observing mode with explicit values merged over defaults. Used for grouping observations by configuration.
enum ObservingModeSummary derives Order:
  // TODO: Update for acquisition customization?
  case GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CentralWavelength,
    ampReadMode:       GmosAmpReadMode,
    roi:               GmosRoi
  ) extends ObservingModeSummary
  // TODO: Update for acquisition customization?
  case GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CentralWavelength,
    ampReadMode:       GmosAmpReadMode,
    roi:               GmosRoi
  ) extends ObservingModeSummary
  // TODO: Update for acquisition customization?
  case Flamingos2LongSlit(
    grating: Flamingos2Disperser,
    filter:  Flamingos2Filter,
    fpu:     Flamingos2Fpu
  ) extends ObservingModeSummary
  case GmosNorthImaging(
    variant:     GmosImagingVariant,
    filters:     NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter],
    ampReadMode: GmosAmpReadMode,
    roi:         GmosRoi
  ) extends ObservingModeSummary
  case GmosSouthImaging(
    variant:     GmosImagingVariant,
    filters:     NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter],
    ampReadMode: GmosAmpReadMode,
    roi:         GmosRoi
  ) extends ObservingModeSummary

  def obsModeType: ObservingModeType = this match
    case GmosNorthLongSlit(_, _, _, _, _, _) => ObservingModeType.GmosNorthLongSlit
    case GmosSouthLongSlit(_, _, _, _, _, _) => ObservingModeType.GmosSouthLongSlit
    case Flamingos2LongSlit(_, _, _)         => ObservingModeType.Flamingos2LongSlit
    case GmosNorthImaging(_, _, _, _)        => ObservingModeType.GmosNorthImaging
    case GmosSouthImaging(_, _, _, _)        => ObservingModeType.GmosSouthImaging

  def toInput: ObservingModeInput = this match
    case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      ObservingModeInput.GmosNorthLongSlit(
        GmosNorthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        )
      )
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      ObservingModeInput.GmosSouthLongSlit(
        GmosSouthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        )
      )
    case Flamingos2LongSlit(disperser, filter, fpu)                                   =>
      ObservingModeInput.Flamingos2LongSlit(
        Flamingos2LongSlitInput(
          disperser = disperser.assign,
          filter = filter.assign,
          fpu = fpu.assign
        )
      )
    case GmosNorthImaging(variant, filters, ampReadMode, roi)                         =>
      ObservingModeInput.GmosNorthImaging(
        GmosNorthImagingInput(
          variant = variant.toInput.assign,
          filters = filters.toList.map(_.toInput).assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        )
      )
    case GmosSouthImaging(variant, filters, ampReadMode, roi)                         =>
      ObservingModeInput.GmosSouthImaging(
        GmosSouthImagingInput(
          // no etm yet
          variant = variant.toInput.assign,
          filters = filters.toList.map(_.toInput).assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        )
      )

object ObservingModeSummary:
  def fromObservingMode(observingMode: ObservingMode): ObservingModeSummary =
    observingMode match
      case n: ObservingMode.GmosNorthLongSlit  =>
        GmosNorthLongSlit(
          n.grating,
          n.filter,
          n.fpu,
          n.centralWavelength,
          n.ampReadMode,
          n.roi
        )
      case s: ObservingMode.GmosSouthLongSlit  =>
        GmosSouthLongSlit(
          s.grating,
          s.filter,
          s.fpu,
          s.centralWavelength,
          s.ampReadMode,
          s.roi
        )
      case f: ObservingMode.Flamingos2LongSlit =>
        Flamingos2LongSlit(f.disperser, f.filter, f.fpu)
      case n: ObservingMode.GmosNorthImaging   =>
        GmosNorthImaging(n.variant, n.filters, n.ampReadMode, n.roi)
      case s: ObservingMode.GmosSouthImaging   =>
        GmosSouthImaging(s.variant, s.filters, s.ampReadMode, s.roi)

  given Display[ObservingModeSummary] = Display.byShortName:
    case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      val cwvStr    = "%.1fnm".format(centralWavelength.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-N Longslit ${grating.shortName} @ $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      val cwvStr    = "%.1fnm".format(centralWavelength.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-S Longslit ${grating.shortName} @ $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName}"
    case Flamingos2LongSlit(grating, filter, fpu)                                     =>
      s"Flamingos2 Longslit ${grating.shortName} ${filter.shortName} ${fpu.shortName}"
    case GmosNorthImaging(variant, filters, ampReadMode, roi)                         =>
      val filterStr = filters.map(_.filter.shortName).toList.mkString(", ")
      s"GMOS-N Imaging ${variant.variantType.display} $filterStr ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthImaging(variant, filters, ampReadMode, roi)                         =>
      val filterStr = filters.map(_.filter.shortName).toList.mkString(", ")
      s"GMOS-S Imaging ${variant.variantType.display} $filterStr ${ampReadMode.shortName} ${roi.shortName}"

  object GmosNorthImaging:
    given Order[GmosNorthImaging] =
      Order.by(x => (x.variant.variantType, x.filters.map(_.filter), x.ampReadMode, x.roi))

  object GmosSouthImaging:
    given Order[GmosSouthImaging] =
      Order.by(x => (x.variant.variantType, x.filters.map(_.filter), x.ampReadMode, x.roi))
