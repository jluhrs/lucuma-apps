// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class GmosSouthSpectroscopySequenceTable(
  visits:       List[Visit.GmosSouth],
  config:       ExecutionConfig.GmosSouth,
  acquisitonSN: Option[SignalToNoiseAt],
  scienceSN:    Option[SignalToNoiseAt],
  isEditing:    IsEditing
) extends ReactFnProps(GmosSouthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosSouth]

object GmosSouthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
