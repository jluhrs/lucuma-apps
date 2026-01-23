// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB.Enums.GmosSouthFilter
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.ImagingSequenceTable

final case class GmosSouthImagingSequenceTable(
  visits:      List[Visit.GmosSouth],
  config:      ExecutionConfig.GmosSouth,
  snPerFilter: Map[GmosSouthFilter, SignalToNoiseAt],
  isEditing:   IsEditing = IsEditing.False
) extends ReactFnProps(GmosSouthImagingSequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
    with ImagingSequenceTable[gmos.DynamicConfig.GmosSouth, GmosSouthFilter]:
  val filterFromDynamicConfig: gmos.DynamicConfig.GmosSouth => Option[GmosSouthFilter] =
    _.filter

object GmosSouthImagingSequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
