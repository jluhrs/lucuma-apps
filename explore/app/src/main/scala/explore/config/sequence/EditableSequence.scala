// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import explore.*
import lucuma.core.model.sequence.InstrumentExecutionConfig

import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import monocle.macros.GenPrism
import monocle.Prism
import monocle.Lens
import monocle.Focus
import monocle.Optional

enum EditableSequence:
  case GmosNorth(
    acquisition: Option[Atom[gmos.DynamicConfig.GmosNorth]],
    science:     Option[List[Atom[gmos.DynamicConfig.GmosNorth]]]
  )
  case GmosSouth(
    acquisition: Option[Atom[gmos.DynamicConfig.GmosSouth]],
    science:     Option[List[Atom[gmos.DynamicConfig.GmosSouth]]]
  )
  case Flamingos2(
    acquisition: Option[Atom[Flamingos2DynamicConfig]],
    science:     Option[List[Atom[Flamingos2DynamicConfig]]]
  )

object EditableSequence: // TODO Types?? Hide behind "InstrumentEditableSequence"?
  import SequenceTileHelper.*

  val gmosNorth: Prism[EditableSequence, EditableSequence.GmosNorth]   =
    GenPrism[EditableSequence, EditableSequence.GmosNorth]
  val gmosSouth: Prism[EditableSequence, EditableSequence.GmosSouth]   =
    GenPrism[EditableSequence, EditableSequence.GmosSouth]
  val flamingos2: Prism[EditableSequence, EditableSequence.Flamingos2] =
    GenPrism[EditableSequence, EditableSequence.Flamingos2]

  object GmosNorth:
    val acquisition: Lens[EditableSequence.GmosNorth, Option[Atom[gmos.DynamicConfig.GmosNorth]]] =
      Focus[EditableSequence.GmosNorth](_.acquisition)
    val science
      : Lens[EditableSequence.GmosNorth, Option[List[Atom[gmos.DynamicConfig.GmosNorth]]]] =
      Focus[EditableSequence.GmosNorth](_.science)

  object GmosSouth:
    val acquisition: Lens[EditableSequence.GmosSouth, Option[Atom[gmos.DynamicConfig.GmosSouth]]] =
      Focus[EditableSequence.GmosSouth](_.acquisition)
    val science
      : Lens[EditableSequence.GmosSouth, Option[List[Atom[gmos.DynamicConfig.GmosSouth]]]] =
      Focus[EditableSequence.GmosSouth](_.science)

  object Flamingos2:
    val acquisition: Lens[EditableSequence.Flamingos2, Option[Atom[Flamingos2DynamicConfig]]]   =
      Focus[EditableSequence.Flamingos2](_.acquisition)
    val science: Lens[EditableSequence.Flamingos2, Option[List[Atom[Flamingos2DynamicConfig]]]] =
      Focus[EditableSequence.Flamingos2](_.science)

  val gmosNorthAcquisition: Optional[EditableSequence, Atom[gmos.DynamicConfig.GmosNorth]]   =
    gmosNorth.andThen(GmosNorth.acquisition).some
  val gmosNorthScience: Optional[EditableSequence, List[Atom[gmos.DynamicConfig.GmosNorth]]] =
    gmosNorth.andThen(GmosNorth.science).some
  val gmosSouthAcquisition: Optional[EditableSequence, Atom[gmos.DynamicConfig.GmosSouth]]   =
    gmosSouth.andThen(GmosSouth.acquisition).some
  val gmosSouthScience: Optional[EditableSequence, List[Atom[gmos.DynamicConfig.GmosSouth]]] =
    gmosSouth.andThen(GmosSouth.science).some
  val flamingos2Acquisition: Optional[EditableSequence, Atom[Flamingos2DynamicConfig]]       =
    flamingos2.andThen(Flamingos2.acquisition).some
  val flamingos2Science: Optional[EditableSequence, List[Atom[Flamingos2DynamicConfig]]]     =
    flamingos2.andThen(Flamingos2.science).some

  def fromLiveSequence(live: LiveSequence): Option[EditableSequence] =
    live.data.toOption
      .flatMap(_._2.map(_.config))
      .collect: // TODO Double check if we really ignore futureSequence for acquisition for all instruments
        case InstrumentExecutionConfig.GmosNorth(execution)  =>
          EditableSequence.GmosNorth(
            acquisition = execution.acquisition.map(a => a.nextAtom),
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
        case InstrumentExecutionConfig.GmosSouth(execution)  =>
          EditableSequence.GmosSouth(
            acquisition = execution.acquisition.map(a => a.nextAtom),
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
        case InstrumentExecutionConfig.Flamingos2(execution) =>
          EditableSequence.Flamingos2(
            acquisition = execution.acquisition.map(a => a.nextAtom),
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
