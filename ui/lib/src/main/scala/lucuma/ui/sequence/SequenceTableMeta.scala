// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import lucuma.core.model.sequence.Step
import japgolly.scalajs.react.Callback

trait SequenceTableMeta[D]:
  def isEditing: IsEditing
  def modRow: Step.Id => Endo[SequenceRow[D]] => Callback
