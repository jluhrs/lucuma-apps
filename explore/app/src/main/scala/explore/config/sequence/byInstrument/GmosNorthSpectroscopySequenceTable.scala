// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.Endo
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class GmosNorthSpectroscopySequenceTable(
  visits:         List[Visit.GmosNorth],
  staticConfig:   gmos.StaticConfig.GmosNorth,
  acquisition:    Option[Atom[gmos.DynamicConfig.GmosNorth]],
  science:        Option[List[Atom[gmos.DynamicConfig.GmosNorth]]],
  acquisitonSN:   Option[SignalToNoiseAt],
  scienceSN:      Option[SignalToNoiseAt],
  isEditing:      IsEditing,
  modAcquisition: Endo[Atom[gmos.DynamicConfig.GmosNorth]] => Callback,
  modScience:     Endo[List[Atom[gmos.DynamicConfig.GmosNorth]]] => Callback
) extends ReactFnProps(GmosNorthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosNorth]

object GmosNorthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
      Instrument.GmosNorth
    )
