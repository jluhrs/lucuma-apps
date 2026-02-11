// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class Flamingos2SequenceTable(
  visits:       List[Visit.Flamingos2],
  staticConfig: Flamingos2StaticConfig,
  acquisition:  Option[Atom[Flamingos2DynamicConfig]],
  science:      Option[List[Atom[Flamingos2DynamicConfig]]],
  acquisitonSN: Option[SignalToNoiseAt],
  scienceSN:    Option[SignalToNoiseAt],
  isEditing:    IsEditing,
  i:            Int // TODO This is a temporary mechanism for demo purposes
) extends ReactFnProps(Flamingos2SequenceTable.component)
    with SequenceTable[Flamingos2StaticConfig, Flamingos2DynamicConfig]
    with SpectroscopySequenceTable[Flamingos2DynamicConfig]

object Flamingos2SequenceTable
    extends SequenceTableBuilder[Flamingos2StaticConfig, Flamingos2DynamicConfig](
      Instrument.Flamingos2
    )
