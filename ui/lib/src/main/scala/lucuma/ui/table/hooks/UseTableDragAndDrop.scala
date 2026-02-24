// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.pragmaticdnd.*
import lucuma.react.table.*
import org.scalajs.dom.HTMLElement

import scala.annotation.unused

type Source[D] = (data: D, height: Int)
type Target[D] = (data: D, edge: Edge)

final case class RowDraggingInfo[D](isDragging: Option[D], isDraggingOver: Option[Target[D]])

final case class UseTableDragAndDrop[D, T, TM, CM, TF](
  rowMod:  ((Row[T, TM, CM, TF], RowDraggingInfo[D]) => TagMod) => (
    Row[T, TM, CM, TF],
    Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]
  ) => VdomNode,
  cellMod: ((Cell[T, Any, TM, CM, TF, Any, Any], RowDraggingInfo[D]) => TagMod) => (
    Cell[T, Any, TM, CM, TF, Any, Any],
    Option[Ref.ToVdom[HTMLElement]],
    TagOf[HTMLElement]
  ) => VdomNode,
  context: Context.Provided[DragAndDropContext]
)

final case class UseVirtualizedTableDragAndDrop[D, T, TM, CM, TF](
  useTableDragAndDrop: UseTableDragAndDrop[D, T, TM, CM, TF],
  containerRef:        Ref.ToVdom[HTMLElement]
):
  export useTableDragAndDrop.*

// Allow skipping the tagMod function if not needed.
extension [D, T, TM, CM, TF](
  rowMod: ((Row[T, TM, CM, TF], RowDraggingInfo[D]) => TagMod) => (
    Row[T, TM, CM, TF],
    Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]
  ) => VdomNode
)
  def apply()
    : (Row[T, TM, CM, TF], Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]) => VdomNode =
    rowMod((_, _) => TagMod.empty)

extension [D, T, TM, CM, TF](
  cellMod: ((Cell[T, Any, TM, CM, TF, Any, Any], RowDraggingInfo[D]) => TagMod) => (
    Cell[T, Any, TM, CM, TF, Any, Any],
    Option[Ref.ToVdom[HTMLElement]],
    TagOf[HTMLElement]
  ) => VdomNode
)
  def apply(): (
    Cell[T, Any, TM, CM, TF, Any, Any],
    Option[Ref.ToVdom[HTMLElement]],
    TagOf[HTMLElement]
  ) => VdomNode =
    cellMod((_, _) => TagMod.empty)

object UseTableDragAndDrop:
  def useTableDragAndDrop[D: cats.Eq, T, TM, CM, TF](
    @unused table: Table[T, TM, CM, TF], // Not used, just to infer type parameters.
    handleColId:   ColumnId,
    getData:       Row[T, TM, CM, TF] => D,
    onDrop:        (D, Option[Target[D]]) => Callback = (_: D, _: Option[Target[D]]) => Callback.empty
  ): HookResult[UseTableDragAndDrop[D, T, TM, CM, TF]] =
    for
      dragging   <- useState[Option[Source[D]]](none)
      dragOver   <- useState[Option[Target[D]]](none)
      dndContext <- useDragAndDropContext[D, D](
                      onDrag = payload =>
                        val data: Option[Data[D]] =
                          payload.location.current.dropTargets.headOption.map(_.data)
                        val edge: Option[Edge]    = data.flatMap(_.extractClosestEdge)
                        dragOver.setState((data.map(_.value), edge).tupled)
                      ,
                      onDrop = payload =>
                        val sourceData: D                     = payload.source.data.value
                        val targetData: Option[Data[D]]       =
                          payload.location.current.dropTargets.headOption.map(_.data)
                        val targetDataEdge: Option[Target[D]] =
                          (targetData.map(_.value), targetData.flatMap(_.extractClosestEdge)).tupled

                        dragging.setState(none) >>
                          dragOver.setState(none) >>
                          onDrop(sourceData, targetDataEdge)
                    )
    yield
      val draggingInfo: RowDraggingInfo[D] =
        RowDraggingInfo(dragging.value.map(_.data), dragOver.value)

      val rowMod =
        (tagMod: (Row[T, TM, CM, TF], RowDraggingInfo[D]) => TagMod) =>
          (
            row:    Row[T, TM, CM, TF],
            render: Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]
          ) =>
            val rowData: D = getData(row)
            dragging.value match
              case Some((data, height)) if data === rowData =>
                <.tr((^.height := s"${height}px").when(dragOver.value.isEmpty)): VdomNode
              case _                                        =>
                DraggableDropTargetWithHandle(
                  handleRef => render(Some(handleRef))(tagMod(row, draggingInfo)),
                  getInitialData = _ => Data(rowData),
                  getData = args => Data(rowData).attachClosestEdge(args, Axis.Vertical.edges),
                  onDraggableDragStart = payload =>
                    dragging.setState(
                      (payload.source.data.value,
                       payload.source.element.getBoundingClientRect().height.toInt
                      ).some
                    )
                ).withKey(s"row-${dndContext.value.get}-$rowData").toUnmounted: VdomNode

      val cellMod =
        (tagMod: (Cell[T, Any, TM, CM, TF, Any, Any], RowDraggingInfo[D]) => TagMod) =>
          (
            cell:    Cell[T, Any, TM, CM, TF, Any, Any],
            context: Option[Ref.ToVdom[HTMLElement]],
            render:  TagOf[HTMLElement]
          ) =>
            val rowData: D = getData(cell.row)

            context
              .filter(_ => cell.column.id == handleColId)
              .map(handleRef => render.withRef(handleRef))
              .getOrElse(render)(
                (dragOver.value, dragging.value) match
                  case (Some((data, Edge.Top)), Some((sData, height)))
                      if data === rowData && data =!= sData => // padding color?
                    TagMod(^.paddingTop := s"${height}px", ^.transitionDuration := "0.1s")
                  case (Some((data, Edge.Bottom)), Some((sData, height)))
                      if data === rowData && data =!= sData =>
                    TagMod(^.paddingBottom := s"${height}px", ^.transitionDuration := "0.1s")
                  case _ => TagMod.empty
              )(tagMod(cell, draggingInfo))

      UseTableDragAndDrop[D, T, TM, CM, TF](rowMod, cellMod, dndContext)

  def useVirtualizedTableDragAndDrop[D: cats.Eq, T, TM, CM, TF](
    @unused table: Table[T, TM, CM, TF], // Not used, just to infer type parameters.
    handleColId:   ColumnId,
    getData:       Row[T, TM, CM, TF] => D,
    onDrop:        (D, Option[Target[D]]) => Callback = (_: D, _: Option[Target[D]]) => Callback.empty
  ): HookResult[UseVirtualizedTableDragAndDrop[D, T, TM, CM, TF]] =
    for
      tableDnd     <- useTableDragAndDrop(table, handleColId, getData, onDrop)
      containerRef <- useAutoScrollRef(getAllowedAxis = _ => Axis.Vertical)
    yield UseVirtualizedTableDragAndDrop(tableDnd, containerRef)
