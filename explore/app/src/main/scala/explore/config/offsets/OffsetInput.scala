// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Offset
import lucuma.react.common.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

final case class OffsetInput(
  id:         NonEmptyString,
  offset:     View[Offset],
  readonly:   Boolean,
  inputClass: Css = Css.Empty,
  labelClass: Css = Css.Empty,
  clazz:      Css = Css.Empty // for the outter div
) extends ReactFnProps(OffsetInput):
  val pId: NonEmptyString = NonEmptyString.unsafeFrom(s"${id.value}-p")
  val qId: NonEmptyString = NonEmptyString.unsafeFrom(s"${id.value}-q")

object OffsetInput
    extends ReactFnComponent[OffsetInput](props =>
      <.div(OffsetGeneratorEditorStyles.OffsetsInput |+| props.clazz)(
        FormInputTextView(
          id = props.pId,
          label = "p:",
          value = props.offset.zoom(Offset.pAngle),
          validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
          changeAuditor = ChangeAuditor.bigDecimal(3.refined, 2.refined),
          placeholder = "0.0",
          disabled = props.readonly,
          inputClass = props.inputClass,
          labelClass = props.labelClass
        ),
        FormInputTextView(
          id = props.qId,
          label = "q:",
          value = props.offset.zoom(Offset.qAngle),
          validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
          changeAuditor = ChangeAuditor.bigDecimal(3.refined, 2.refined),
          placeholder = "0.0",
          disabled = props.readonly,
          inputClass = props.inputClass,
          labelClass = props.labelClass
        )
      )
    )
