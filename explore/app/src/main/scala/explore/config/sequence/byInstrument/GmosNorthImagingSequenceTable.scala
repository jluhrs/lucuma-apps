// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.ImagingSequenceTable

final case class GmosNorthImagingSequenceTable(
  visits:      List[Visit.GmosNorth],
  config:      ExecutionConfig.GmosNorth,
  snPerFilter: Map[GmosNorthFilter, SignalToNoiseAt],
  isEditing:   IsEditing,
  i:           Int // TODO This is a temporary mechanism for demo purposes
) extends ReactFnProps(GmosNorthImagingSequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
    with ImagingSequenceTable[gmos.DynamicConfig.GmosNorth, GmosNorthFilter]:
  val filterFromDynamicConfig: gmos.DynamicConfig.GmosNorth => Option[GmosNorthFilter] =
    _.filter

object GmosNorthImagingSequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
      Instrument.GmosNorth
    )
