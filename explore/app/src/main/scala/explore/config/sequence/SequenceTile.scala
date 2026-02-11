// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.components.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.sequence.byInstrument.*
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.TooltipOptions
import lucuma.refined.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.model.Visit
import lucuma.ui.primereact.*
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.SequenceData
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet
import org.scalajs.dom.HTMLElement
import lucuma.core.model.sequence.Atom
import japgolly.scalajs.react.vdom.TagOf
import lucuma.core.model.sequence.ExecutionConfig
import monocle.Optional

final case class SequenceTile(
  obsId:               Observation.Id,
  obsExecution:        Execution,
  asterismIds:         SortedSet[Target.Id],
  customSedTimestamps: List[Timestamp],
  calibrationRole:     Option[CalibrationRole],
  sequenceChanged:     View[Pot[Unit]],
  isEditing:           View[IsEditing]
) extends Tile[SequenceTile](ObsTabTileIds.SequenceId.id, "Sequence", canMinimize = !isEditing.get)(
      SequenceTile // TODO Move isEditing state here, but we need to be able to change tile state from within tile
    )

object SequenceTile
    extends TileComponent[SequenceTile]((props, sizeState) =>
      import SequenceTileHelper.*

      for
        i                <- useStateView(0) // TODO This is a temporary mechanism for demo purposes
        liveSequence     <- useLiveSequence(
                              props.obsId,
                              props.asterismIds.toList,
                              props.customSedTimestamps,
                              props.calibrationRole
                            )
        _                <- useEffectWithDeps(liveSequence.data): dataPot =>
                              props.sequenceChanged.set(dataPot.void)
        editableSequence <- useStateView(EditableSequence.fromLiveSequence(liveSequence))
        _                <- useEffect:      // Keep editable sequence in sync with live sequence when not editing
                              editableSequence
                                .set(EditableSequence.fromLiveSequence(liveSequence))
                                .unless_(props.isEditing.get)
      yield
        val execution: Execution           = props.obsExecution
        val staleCss: TagMod               = execution.digest.staleClass
        val staleTooltip: Option[VdomNode] = execution.digest.staleTooltip
        val programTimeCharge: TimeSpan    = execution.programTimeCharge.value
        val executed: TagOf[HTMLElement]   = timeDisplay("Executed", programTimeCharge)

        def resolveAcquisition[S, D](
          config:           ExecutionConfig[S, D],
          editableOptional: Optional[EditableSequence, Atom[D]]
        ): Option[Atom[D]] = // For acquisition, we ignore possibleFuture
          if props.isEditing.get then editableSequence.get.flatMap(editableOptional.getOption)
          else config.acquisition.map(_.nextAtom)

        def resolveScience[S, D](
          config:           ExecutionConfig[S, D],
          editableOptional: Optional[EditableSequence, List[Atom[D]]]
        ): Option[List[Atom[D]]] =
          if props.isEditing.get then editableSequence.get.flatMap(editableOptional.getOption)
          else config.science.map(a => a.nextAtom +: a.possibleFuture)

        val title =
          <.span(
            execution.digest.programTimeEstimate.value
              .map: plannedTime =>
                val total: TimeSpan             = programTimeCharge +| plannedTime
                val pending: TagOf[HTMLElement] =
                  timeDisplay(
                    "Pending",
                    plannedTime,
                    timeClass = staleCss,
                    timeTooltip = staleTooltip
                  )
                val planned: TagOf[HTMLElement] =
                  timeDisplay("Planned", total, timeClass = staleCss, timeTooltip = staleTooltip)

                <.span(ExploreStyles.SequenceTileTitle)(
                  <.span(ExploreStyles.SequenceTileTitleSide),
                  <.span(ExploreStyles.SequenceTileTitleSummary)(
                    HelpIcon("target/main/sequence-times.md".refined),
                    planned,
                    executed,
                    pending
                  ),
                  <.span(
                    ExploreStyles.SequenceTileTitleSide,
                    ExploreStyles.SequenceTileTitleEdit
                  )(
                    Button(
                      onClick = props.isEditing.set(IsEditing.True),
                      label = "Edit",
                      icon = Icons.Pencil,
                      tooltip = "Enter sequence editing mode",
                      tooltipOptions = TooltipOptions.Top
                    ).mini.compact.when(!props.isEditing.get && sizeState.isMaximized),
                    React
                      .Fragment(
                        Button(
                          onClick = props.isEditing.set(IsEditing.False),
                          label = "Cancel",
                          icon = Icons.Close,
                          tooltip = "Cancel sequence editing",
                          tooltipOptions = TooltipOptions.Top,
                          severity = Button.Severity.Danger
                        ).mini.compact,
                        Button(
                          onClick = props.isEditing.set(IsEditing.False) >> i.mod(_ + 1),
                          label = "Accept",
                          icon = Icons.Checkmark,
                          tooltip = "Accept sequence modifications",
                          tooltipOptions = TooltipOptions.Top,
                          severity = Button.Severity.Success
                        ).mini.compact
                      )
                      .when(props.isEditing.get)
                  )
                )
              .getOrElse(executed)
          )

        val mismatchError = Message(
          text = "ERROR: Sequence and S/N are inconsistent.",
          severity = Message.Severity.Error
        )

        val body =
          props.sequenceChanged.get
            .flatMap(_ => liveSequence.data)
            .renderPot(
              (visitsOpt, sequenceDataOpt) =>
                // TODO Show visits even if sequence data is not available
                sequenceDataOpt
                  .fold[VdomNode](
                    Message(
                      text = "Empty or incomplete sequence data returned by server",
                      severity = Message.Severity.Error
                    )
                  ) {
                    case SequenceData(InstrumentExecutionConfig.GmosNorth(config), signalToNoise) =>
                      val visits: List[Visit.GmosNorth] =
                        visitsOpt
                          .collect:
                            case ExecutionVisits.GmosNorth(vs) => vs.toList
                          .orEmpty

                      signalToNoise match
                        case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                          GmosNorthSpectroscopySequenceTable(
                            visits,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosNorthAcquisition),
                            resolveScience(config, EditableSequence.gmosNorthScience),
                            acquisitionSn,
                            scienceSn,
                            props.isEditing.get,
                            i.get
                          )
                        case ModeSignalToNoise.GmosNorthImaging(snPerFilter)          =>
                          GmosNorthImagingSequenceTable(
                            visits,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosNorthAcquisition),
                            resolveScience(config, EditableSequence.gmosNorthScience),
                            snPerFilter,
                            props.isEditing.get,
                            i.get
                          )
                        case _                                                        => mismatchError
                    case SequenceData(InstrumentExecutionConfig.GmosSouth(config), signalToNoise) =>
                      val visits: List[Visit.GmosSouth] =
                        visitsOpt
                          .collect:
                            case ExecutionVisits.GmosSouth(vs) => vs.toList
                          .orEmpty

                      signalToNoise match
                        case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                          GmosSouthSpectroscopySequenceTable(
                            visits,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosSouthAcquisition),
                            resolveScience(config, EditableSequence.gmosSouthScience),
                            acquisitionSn,
                            scienceSn,
                            props.isEditing.get,
                            i.get
                          )
                        case ModeSignalToNoise.GmosSouthImaging(snPerFilter)          =>
                          GmosSouthImagingSequenceTable(
                            visits,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosSouthAcquisition),
                            resolveScience(config, EditableSequence.gmosSouthScience),
                            snPerFilter,
                            props.isEditing.get,
                            i.get
                          )
                        case _                                                        => mismatchError
                    case SequenceData(
                          InstrumentExecutionConfig.Flamingos2(config),
                          ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                        ) =>
                      Flamingos2SequenceTable(
                        visitsOpt
                          .collect:
                            case ExecutionVisits.Flamingos2(visits) => visits.toList
                          .orEmpty,
                        config.static,
                        resolveAcquisition(config, EditableSequence.flamingos2Acquisition),
                        resolveScience(config, EditableSequence.flamingos2Science),
                        acquisitionSn,
                        scienceSn,
                        props.isEditing.get,
                        i.get
                      )
                    case _                                                                        => mismatchError
                  },
              errorRender = m =>
                <.div(ExploreStyles.SequencesPanelError)(
                  Message(
                    text = m.getMessage,
                    severity = Message.Severity.Warning,
                    icon = Icons.ExclamationTriangle
                  )
                )
            )

        TileContents(title, body)
    )
