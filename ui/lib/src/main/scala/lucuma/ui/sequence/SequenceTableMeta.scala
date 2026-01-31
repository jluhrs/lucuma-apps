// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import japgolly.scalajs.react.Callback
import lucuma.core.enums.SequenceType

trait SequenceTableMeta[D]:
  def isEditing: IsEditing
  def modAcquisition: Endo[List[SequenceRow[D]]] => Callback
  def modScience: Endo[List[SequenceRow[D]]] => Callback

  def seqTypeMod(seqType: SequenceType): Endo[List[SequenceRow[D]]] => Callback =
    seqType match
      case SequenceType.Acquisition => modAcquisition
      case SequenceType.Science     => modScience
