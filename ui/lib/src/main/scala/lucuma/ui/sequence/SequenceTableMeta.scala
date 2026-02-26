// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import japgolly.scalajs.react.Callback
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom

trait SequenceTableMeta[D]:
  def isEditing: IsEditing
  def modAcquisition: Endo[Atom[D]] => Callback
  def modScience: Endo[List[Atom[D]]] => Callback

  def seqTypeMod(seqType: SequenceType): Endo[List[Atom[D]]] => Callback =
    seqType match
      case SequenceType.Acquisition =>
        modAtomList => modAcquisition(atom => modAtomList(List(atom)).head)
      case SequenceType.Science     => modScience
