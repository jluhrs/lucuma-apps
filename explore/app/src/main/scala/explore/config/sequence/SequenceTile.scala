// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.given
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
import lucuma.react.primereact.Message
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

final case class SequenceTile(
  obsId:               Observation.Id,
  obsExecution:        Execution,
  asterismIds:         SortedSet[Target.Id],
  customSedTimestamps: List[Timestamp],
  calibrationRole:     Option[CalibrationRole],
  sequenceChanged:     View[Pot[Unit]],
  isEditing:           View[IsEditing]
) extends Tile[SequenceTile](ObsTabTileIds.SequenceId.id, "Sequence")(SequenceTile)

object SequenceTile
    extends TileComponent[SequenceTile]((props, _) =>
      import SequenceTileHelper.*

      for
        i            <- useStateView(0) // TODO This is a temporary mechanism for demo purposes
        liveSequence <- useLiveSequence(
                          props.obsId,
                          props.asterismIds.toList,
                          props.customSedTimestamps,
                          props.calibrationRole
                        )
        _            <- useEffectWithDeps(liveSequence.data): dataPot =>
                          props.sequenceChanged.set(dataPot.void)
      yield
        val title =
          <.span(ExploreStyles.SequenceTileTitle) {
            val execution         = props.obsExecution
            val staleCss          = execution.digest.staleClass
            val staleTooltip      = execution.digest.staleTooltip
            val programTimeCharge = execution.programTimeCharge.value

            val executed = timeDisplay("Executed", programTimeCharge)

            execution.digest.programTimeEstimate.value
              .map: plannedTime =>
                val total   = programTimeCharge +| plannedTime
                val pending = timeDisplay(
                  "Pending",
                  plannedTime,
                  timeClass = staleCss,
                  timeTooltip = staleTooltip
                )
                val planned =
                  timeDisplay("Planned", total, timeClass = staleCss, timeTooltip = staleTooltip)

                <.span(
                  ^.width := "100%",
                  ^.display.flex,
                  ^.justifyContent.spaceBetween /*, ^.gap := "1rem"*/
                )(
                  <.span(^.flex := "1 1 0"),
                  <.span(
                    // ^.flex := "1 1 0",
                    ^.display.flex,
                    ^.justifyContent.center /*, ^.gap := "1rem"*/
                  )(
                    HelpIcon("target/main/sequence-times.md".refined),
                    planned,
                    executed,
                    pending
                  ),
                  <.span(^.flex := "1 1 0",
                         ^.display.flex,
                         ^.justifyContent.flexEnd /*, ^.gap := "1rem"*/
                  )(
                    Button(
                      onClick = props.isEditing.set(IsEditing.True),
                      label = "Edit",
                      icon = Icons.Pencil,
                      tooltip = "Enter sequence editing mode",
                      tooltipOptions = TooltipOptions.Top
                    ).mini.compact.when(!props.isEditing.get),
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
          }

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
                            config,
                            acquisitionSn,
                            scienceSn,
                            props.isEditing,
                            props.i
                          )
                        case ModeSignalToNoise.GmosNorthImaging(snPerFilter)          =>
                          GmosNorthImagingSequenceTable(
                            visits,
                            config,
                            snPerFilter,
                            props.isEditing,
                            props.i
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
                            config,
                            acquisitionSn,
                            scienceSn,
                            props.isEditing,
                            props.i
                          )
                        case ModeSignalToNoise.GmosSouthImaging(snPerFilter)          =>
                          GmosSouthImagingSequenceTable(
                            visits,
                            config,
                            snPerFilter,
                            props.isEditing,
                            props.i
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
                        config,
                        acquisitionSn,
                        scienceSn,
                        props.isEditing,
                        props.i
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
