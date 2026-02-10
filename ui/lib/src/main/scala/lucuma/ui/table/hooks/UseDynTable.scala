// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import japgolly.scalajs.react.*
import lucuma.react.SizePx
import lucuma.react.table.*
import lucuma.ui.reusability.given
import lucuma.ui.table.ColumnSize
import lucuma.ui.table.ColumnSize.*

/**
 * Provides values to be passed to the table definition:
 *   - `setInitialColWidths`: Convenience method to set column size for all colums to the ones
 *     passed to the hook. Eg: `cols.map(useDynTable.setInitialColWidths)`.
 *   - `columnSizing`: To be passed directly to table `state` as `PartialTableState.columnSizing`.
 *   - `columnVisibility`: To be passed directly to table `state` as
 *     `PartialTableState.columnVisibility`.
 *   - `onColumnSizingChangeHandler`: To be passed directly to table `onColumnSizingChange`.
 *
 * Make sure the table has `table-layout: fixed`.
 */
class UseDynTable(
  initialColumnSizes:                  Map[ColumnId, ColumnSize],
  colState:                            DynTable.ColState,
  val onColumnSizingChangeHandler:     Updater[ColumnSizing] => Callback,
  val onColumnVisibilityChangeHandler: Updater[ColumnVisibility] => Callback
):
  def setInitialColWidths[R, TM, CM, TF](
    cols: List[ColumnDef[R, ?, TM, CM, TF, ?, ?]]
  ): List[ColumnDef[R, ?, TM, CM, TF, ?, ?]] =
    cols.map:
      case col @ ColumnDef.Single(_) => col.withColumnSize(initialColumnSizes(col.id))
      case col @ ColumnDef.Group(_)  => col.withColumnSize(initialColumnSizes(col.id))

  export colState.{computedVisibility => columnVisibility, resized => columnSizing}

object UseDynTable:
  def useDynTable(dynTableDef: DynTable, width: SizePx): HookResult[UseDynTable] =
    for
      colState <- useState(dynTableDef.initialState)
      _        <- useEffectWithDeps(width): w => // Recompute columns upon resize
                    CallbackTo(dynTableDef.adjustColSizes(w)(colState.value)) >>= colState.setState
    yield
      def onColumnSizingChangeHandler(updater: Updater[ColumnSizing]): Callback =
        colState.modState: oldState =>
          dynTableDef.adjustColSizes(width):
            updater match
              case Updater.Set(v)  => DynTable.ColState.resized.replace(v)(oldState)
              case Updater.Mod(fn) => DynTable.ColState.resized.modify(fn)(oldState)

      def onColumnVisibilityChangeHandler(updater: Updater[ColumnVisibility]): Callback =
        colState.modState: oldState =>
          dynTableDef.adjustColSizes(width):
            updater match
              case Updater.Set(v)  => DynTable.ColState.visibility.replace(v)(oldState)
              case Updater.Mod(fn) => DynTable.ColState.visibility.modify(fn)(oldState)

      UseDynTable(
        dynTableDef.columnSizes,
        colState.value,
        onColumnSizingChangeHandler,
        onColumnVisibilityChangeHandler
      )
