// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.UnnormalizedSED
import lucuma.schemas.ObservationDB.Types.UnnormalizedSedInput
import lucuma.schemas.odb.input.*

private def initialUnnormalizedSedInput(sed: Option[UnnormalizedSED]): UnnormalizedSedInput =
  // if we don't yet have an SED, I don't think it matters which one it is.
  sed.fold(UnnormalizedSedInput.StellarLibrary(StellarLibrarySpectrum.O5V))(_.toInput)
