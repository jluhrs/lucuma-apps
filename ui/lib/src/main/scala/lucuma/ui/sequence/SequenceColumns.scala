// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.InputNumber
import lucuma.react.primereact.TooltipOptions
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.format.formatSN
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*

import SequenceRowFormatters.*

// `T` is the actual type of the table row, from which we extract an `R` using `getStep`.
// `D` is the `DynamicConfig`.
// `TM` is the type of the table meta.
// `CM` is the type of the column meta.
// `TF` is the type of the global filter.
class SequenceColumns[D, T, R <: SequenceRow[D], TM <: SequenceTableMeta[D], CM, TF](
  colDef:          ColumnDef.Applied[Expandable[HeaderOrRow[T]], TM, CM, TF],
  getStepFromRow:  T => Option[R],
  getIndexFromRow: T => Option[StepIndex]
) extends SequenceEditOptics[D, T, R, TM, CM, TF](getStepFromRow):
  private lazy val editControlsCol: colDef.TypeFor[Boolean] =
    colDef(
      SequenceColumns.EditControlsColumnId,
      _.getStep.map(_.isFinished).getOrElse(true),
      header = "",
      cell = c =>
        val isFinished: Boolean = c.value
        <.span(SequenceStyles.EditControlsCell)(
          Button(
            icon = SequenceIcons.Clone,
            clazz = SequenceStyles.CloneButton,
            onClick = c.getStep.foldMap(step => handleAllSeqTypesRowEditAsync(c)(cloneRow(step))),
            tooltip = "Clone Step",
            tooltipOptions = TooltipOptions.Top
          ).mini.compact,
          Button(
            icon = SequenceIcons.Trash,
            clazz = SequenceStyles.DeleteButton,
            onClick = c.getFutureStep.foldMap(step => handleRowEdit(c)(deleteRow(step.stepId))),
            tooltip = "Remove Step",
            tooltipOptions = TooltipOptions.Top
          ).mini.compact.unless(isFinished)
        )
    )

  private lazy val indexAndTypeCol: colDef.TypeFor[(Option[StepIndex], Option[StepTypeDisplay])] =
    colDef(
      SequenceColumns.IndexAndTypeColumnId,
      _.value.toOption
        .map(row => (getIndexFromRow(row), getStepFromRow(row).flatMap(_.stepTypeDisplay)))
        .getOrElse((none, none)),
      header = "Step",
      cell = c =>
        React.Fragment(
          c.value._1.map(_.value.value),
          c.value._2.map(_.renderVdom)
        )
    )

  private lazy val exposureCol: colDef.TypeFor[Option[TimeSpan]] =
    colDef(
      SequenceColumns.ExposureColumnId,
      _.getStep.flatMap(_.exposureTime),
      header = _ => "Exp (sec)",
      cell = c =>
        val isEditing: Boolean  = c.table.options.meta.exists(_.isEditing.value)
        val isFinished: Boolean = c.getStep.forall(_.isFinished)
        (c.value, c.getStep.flatMap(_.instrument))
          .mapN[VdomNode]: (v, i) =>
            if isEditing && !isFinished then
              React.Fragment(
                InputNumber( // TODO Decimals according to instrument
                  id = s"exposure-${c.row.index}",
                  value = v.toSeconds.toDouble,
                  onValueChange = e =>
                    handleRowValueEdit(c)(exposureReplace): // TODO Can we do this better than cast?
                      TimeSpan.fromSeconds(e.value.get.asInstanceOf[Double].toLong)
                  ,
                  clazz = SequenceStyles.SequenceInput
                )
              )
            else FormatExposureTime(i)(v).value
    )

  private lazy val guideStateCol: colDef.TypeFor[Option[Boolean]] =
    colDef(
      SequenceColumns.GuideColumnId,
      _.getStep.map(_.hasGuiding),
      header = "",
      cell = _.value
        .filter(identity) // Only render on Some(true)
        .map(_ => SequenceIcons.Crosshairs.withClass(SequenceStyles.StepGuided))
    )

  private lazy val pOffsetCol: colDef.TypeFor[Option[Offset.P]] =
    colDef(
      SequenceColumns.PColumnId,
      _.getStep.flatMap(_.offset.map(_.p)),
      header = _ => "p",
      cell = _.value.map(FormatOffsetP(_).value).orEmpty
    )

  private lazy val qOffsetCol: colDef.TypeFor[Option[Offset.Q]] =
    colDef(
      SequenceColumns.QColumnId,
      _.getStep.flatMap(_.offset.map(_.q)),
      header = _ => "q",
      cell = _.value.map(FormatOffsetQ(_).value).orEmpty
    )

  private lazy val wavelengthCol: colDef.TypeFor[Option[Wavelength]] =
    colDef(
      SequenceColumns.WavelengthColumnId,
      _.getStep.flatMap(_.wavelength),
      header = _ => "λ (nm)",
      cell = _.value.map(FormatWavelength(_).value).getOrElse("-")
    )

  private lazy val fpuCol: colDef.TypeFor[Option[String]] =
    colDef(
      SequenceColumns.FPUColumnId,
      _.getStep.flatMap(_.fpuName),
      header = _ => "FPU",
      cell = _.value.orEmpty
    )

  private lazy val gratingCol: colDef.TypeFor[Option[String]] =
    colDef(
      SequenceColumns.GratingColumnId,
      _.getStep.flatMap(_.gratingName),
      header = "Grating",
      cell = _.value.orEmpty
    )

  private lazy val filterCol: colDef.TypeFor[Option[String]] =
    colDef(
      SequenceColumns.FilterColumnId,
      _.getStep.flatMap(_.filterName),
      header = "Filter",
      cell = _.value.orEmpty
    )

  private lazy val xBinCol: colDef.TypeFor[Option[String]] =
    colDef(
      SequenceColumns.XBinColumnId,
      _.getStep.flatMap(_.readoutXBin),
      header = _ => "Xbin",
      cell = _.value.orEmpty
    )

  private lazy val yBinCol: colDef.TypeFor[Option[String]] =
    colDef(
      SequenceColumns.YBinColumnId,
      _.getStep.flatMap(_.readoutYBin),
      header = _ => "Ybin",
      cell = _.value.orEmpty
    )

  private lazy val roiCol: colDef.TypeFor[Option[String]] =
    colDef(
      SequenceColumns.ROIColumnId,
      _.getStep.flatMap(_.roi),
      header = "ROI",
      cell = _.value.orEmpty
    )
  colDef(
    SequenceColumns.ROIColumnId,
    _.getStep.flatMap(_.roi),
    header = "ROI",
    cell = _.value.orEmpty
  )

  private lazy val snCol: colDef.TypeFor[Option[SignalToNoise]] =
    colDef(
      SequenceColumns.SNColumnId,
      _.getStep.flatMap(_.signalToNoise),
      header = "S/N",
      cell = _.value.map(formatSN).orEmpty
    )

  private lazy val readModeCol: colDef.TypeFor[Option[String]] =
    colDef(
      ColumnId("readMode"),
      _.getStep.flatMap(_.readMode),
      header = "Read Mode",
      cell = _.value.orEmpty
    )

  lazy val ForGmos: List[colDef.TypeFor[?]] =
    List(
      editControlsCol,
      indexAndTypeCol,
      exposureCol,
      guideStateCol,
      pOffsetCol,
      qOffsetCol,
      wavelengthCol,
      fpuCol,
      gratingCol,
      filterCol,
      xBinCol,
      yBinCol,
      roiCol,
      snCol
    )

  lazy val ForFlamingos2: List[colDef.TypeFor[?]] =
    List(
      editControlsCol,
      indexAndTypeCol,
      exposureCol,
      guideStateCol,
      pOffsetCol,
      qOffsetCol,
      wavelengthCol,
      fpuCol,
      gratingCol,
      filterCol,
      readModeCol,
      snCol
    )

  def apply(instrument: Instrument): List[colDef.TypeFor[?]] =
    instrument match
      case Instrument.GmosNorth | Instrument.GmosSouth => ForGmos
      case Instrument.Flamingos2                       => ForFlamingos2
      case _                                           => throw new Exception(s"Unimplemented instrument: $instrument")

object SequenceColumns:
  val EditControlsColumnId: ColumnId = ColumnId("editControls")
  val IndexAndTypeColumnId: ColumnId = ColumnId("stepType")
  val ExposureColumnId: ColumnId     = ColumnId("exposure")
  val GuideColumnId: ColumnId        = ColumnId("guide")
  val PColumnId: ColumnId            = ColumnId("p")
  val QColumnId: ColumnId            = ColumnId("q")
  val WavelengthColumnId: ColumnId   = ColumnId("lambda")
  val FPUColumnId: ColumnId          = ColumnId("fpu")
  val GratingColumnId: ColumnId      = ColumnId("grating")
  val FilterColumnId: ColumnId       = ColumnId("filter")
  val XBinColumnId: ColumnId         = ColumnId("xbin")
  val YBinColumnId: ColumnId         = ColumnId("ybin")
  val ROIColumnId: ColumnId          = ColumnId("roi")
  val ReadModeColumnId: ColumnId     = ColumnId("readMode")
  val SNColumnId: ColumnId           = ColumnId("sn")

  object BaseColumnSizes {
    private val CommonColumnSizes: Map[ColumnId, ColumnSize] = Map(
      EditControlsColumnId -> FixedSize(70.toPx),
      IndexAndTypeColumnId -> FixedSize(60.toPx),
      ExposureColumnId     -> Resizable(77.toPx, min = 77.toPx, max = 130.toPx),
      GuideColumnId        -> FixedSize(36.toPx),
      PColumnId            -> FixedSize(75.toPx),
      QColumnId            -> FixedSize(75.toPx),
      WavelengthColumnId   -> Resizable(75.toPx, min = 75.toPx, max = 130.toPx),
      FPUColumnId          -> Resizable(132.toPx, min = 132.toPx),
      GratingColumnId      -> Resizable(120.toPx, min = 120.toPx),
      FilterColumnId       -> Resizable(90.toPx, min = 90.toPx),
      SNColumnId           -> Resizable(75.toPx, min = 75.toPx, max = 130.toPx)
    )

    val ForGmos: Map[ColumnId, ColumnSize] =
      CommonColumnSizes ++ Map(
        XBinColumnId -> FixedSize(60.toPx),
        YBinColumnId -> FixedSize(60.toPx),
        ROIColumnId  -> Resizable(75.toPx, min = 75.toPx)
      )

    val ForFlamingos2: Map[ColumnId, ColumnSize] =
      CommonColumnSizes ++ Map(
        ReadModeColumnId -> Resizable(75.toPx, min = 75.toPx)
      )

    def apply(instrument: Instrument): Map[ColumnId, ColumnSize] =
      instrument match
        case Instrument.GmosNorth | Instrument.GmosSouth => ForGmos
        case Instrument.Flamingos2                       => ForFlamingos2
        case _                                           => throw new Exception(s"Unimplemented instrument: $instrument")
  }

  // The order in which they are removed by overflow. The ones at the beginning go first.
  // Missing columns are not removed by overflow. (We declare them in reverse order)
  object BaseColumnPriorities {
    val ForGmos: List[ColumnId] = List(
      PColumnId,
      QColumnId,
      GuideColumnId,
      ExposureColumnId,
      SNColumnId,
      ROIColumnId,
      XBinColumnId,
      YBinColumnId,
      FilterColumnId,
      GratingColumnId,
      FPUColumnId
    ).reverse

    val ForFlamingos2: List[ColumnId] = List(
      PColumnId,
      QColumnId,
      GuideColumnId,
      ExposureColumnId,
      SNColumnId,
      FilterColumnId,
      GratingColumnId,
      FPUColumnId
    ).reverse

    def apply(instrument: Instrument): List[ColumnId] =
      instrument match
        case Instrument.GmosNorth | Instrument.GmosSouth => ForGmos
        case Instrument.Flamingos2                       => ForFlamingos2
        case _                                           => throw new Exception(s"Unimplemented instrument: $instrument")
  }

  def headerCell[T, R, TM, CM, FM](
    colId:  ColumnId,
    colDef: ColumnDef.Applied[Expandable[HeaderOrRow[T]], TM, CM, FM]
  ): colDef.TypeFor[Option[VdomNode]] =
    colDef(
      colId,
      _.value.left.toOption.map(_.content),
      header = "",
      cell = cell =>
        cell.value
          .map: header =>
            <.span(
              SequenceStyles.TableHeader,
              TagMod(
                SequenceStyles.TableHeaderExpandable,
                ^.onClick ==>
                  (_.stopPropagationCB >> cell.row.getToggleExpandedHandler()),
                <.span(
                  TableStyles.ExpanderChevron,
                  TableStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded())
                )(TableIcons.ChevronRight.withFixedWidth())
              ).when(cell.row.getCanExpand()),
              <.span(SequenceStyles.TableHeaderContent)(header)
            ),
      enableResizing = false
    )
