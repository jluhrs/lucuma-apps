// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.demo

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.table.hooks.*

object TableDemo:
  case class Details(year: Int, pickups: Int, color: String)
  object Details:
    given Reusability[Details] = Reusability.by_==

  case class Guitar(id: Int, make: String, model: String, details: Details)
  object Guitar:
    given Reusability[Guitar] = Reusability.by_==

  private val ColDef = ColumnDef[Guitar]

  private val Columns =
    Reusable.always:
      List(
        ColDef(ColumnId("handle"), cell = _ => <.span(^.fontSize.large, "≡")).withSize(20.toPx),
        ColDef(ColumnId("id"), _.id, "Id", ctx => s"g-${ctx.value}"),
        ColDef(ColumnId("make"), _.make, _ => "Make"),
        ColDef(ColumnId("model"), _.model, _ => "Model"),
        ColDef.group(
          ColumnId("details"),
          _ => <.div(^.textAlign.center)("Details"),
          List(
            ColDef(ColumnId("year"), _.details.year, _ => "Year"),
            ColDef(ColumnId("pickups"), _.details.pickups, _ => "Pickups"),
            ColDef(ColumnId("color"), _.details.color, _ => "Color")
          )
        )
      )

  val guitars =
    Reusable.always:
      List(
        Guitar(1, "Fender", "Stratocaster", Details(2019, 3, "Sunburst")),
        Guitar(2, "Gibson", "Les Paul", Details(1958, 2, "Gold top")),
        Guitar(3, "Fender", "Telecaster", Details(1971, 2, "Ivory")),
        Guitar(4, "Godin", "LG", Details(2008, 2, "Burgundy"))
      )

  val component =
    ScalaFnComponent[Unit]: _ =>
      for
        table    <- useReactTable:
                      TableOptions(Columns, guitars, enableColumnResizing = true)
        tableDnd <- useTableDragAndDrop(
                      table,
                      getData = _.original.id,
                      onDrop = (sourceData, target) =>
                        Callback.log:
                          s"Dropped $sourceData on: $target"
                    )
      yield tableDnd.context(
        <.h2("Drag and drop table"),
        HTMLTable(
          table,
          Css("guitars"),
          rowMod = tableDnd.rowMod((c, draggingInfo) =>
            (^.boxShadow := "inset 0 -2px green")
              .unless(draggingInfo.isDragging.contains(c.original.id))
          ),
          cellMod = tableDnd.cellMod()
        )
      )
