// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import explore.*
import explore.components.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.config.sequence.byInstrument.*
import explore.givens.given
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.undo.UndoStacks
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
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
import monocle.Iso
import monocle.Optional
import org.scalajs.dom.HTMLElement

import scala.collection.immutable.SortedSet

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
        liveSequence     <- useLiveSequence(
                              props.obsId,
                              props.asterismIds.toList,
                              props.customSedTimestamps,
                              props.calibrationRole
                            )
        _                <-
          useEffectWithDeps(liveSequence.visits, liveSequence.sequence): (visitsPot, sequencePot) =>
            props.sequenceChanged.set((visitsPot.value, sequencePot.value.map(_.get)).tupled.void)
        editableSequence <- useStateView(EditableSequence.fromLiveSequence(liveSequence))
        _                <-
          useEffectWithDeps(liveSequence): newLiveSequence =>
            editableSequence // Keep editable sequence in sync with live sequence when not editing
              .set(EditableSequence.fromLiveSequence(newLiveSequence))
              .unless_(props.isEditing.get)
        undoStacks       <- useStateView(UndoStacks.empty[IO, Option[EditableSequence]])
        _                <- useEffectWithDeps(props.isEditing.get.value): _ =>
                              undoStacks.set(UndoStacks.empty[IO, Option[EditableSequence]])
      yield
        val execution: Execution           = props.obsExecution
        val staleCss: TagMod               = execution.digest.staleClass
        val staleTooltip: Option[VdomNode] = execution.digest.staleTooltip
        val programTimeCharge: TimeSpan    = execution.programTimeCharge.value
        val executed: TagOf[HTMLElement]   = timeDisplay("Executed", programTimeCharge)

        val undoCtx: UndoContext[Option[EditableSequence]] =
          UndoContext(undoStacks, editableSequence)

        def replaceAcquisition[S, D](
          editableOptional:        Optional[EditableSequence, Atom[D]],
          executionConfigOptional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]]
        ): Endo[InstrumentExecutionConfig] =
          editableSequence.get
            .flatMap(editableOptional.getOption)
            .foldMap: newAcq =>
              executionConfigOptional
                .andThen(ExecutionConfig.acquisition.some)
                .andThen(ExecutionSequence.nextAtom)
                .replace(newAcq)

        val replaceAllAcquisitions: Endo[InstrumentExecutionConfig] =
          replaceAcquisition(
            EditableSequence.gmosNorthAcquisition,
            InstrumentExecutionConfig.gmosNorth
              .andThen(InstrumentExecutionConfig.GmosNorth.executionConfig)
          ) >>>
            replaceAcquisition(
              EditableSequence.gmosSouthAcquisition,
              InstrumentExecutionConfig.gmosSouth
                .andThen(InstrumentExecutionConfig.GmosSouth.executionConfig)
            ) >>>
            replaceAcquisition(
              EditableSequence.flamingos2Acquisition,
              InstrumentExecutionConfig.flamingos2
                .andThen(InstrumentExecutionConfig.Flamingos2.executionConfig)
            )

        def replaceScience[S, D](
          editableOptional:        Optional[EditableSequence, List[Atom[D]]],
          executionConfigOptional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]]
        ): Endo[InstrumentExecutionConfig] =
          editableSequence.get
            .flatMap(editableOptional.getOption)
            .foldMap: newScience =>
              executionConfigOptional
                .andThen(ExecutionConfig.science.some)
                .andThen(ExecutionSequence.nextAtom)
                .replace(newScience.head) >>>
                executionConfigOptional
                  .andThen(ExecutionConfig.science.some)
                  .andThen(ExecutionSequence.possibleFuture)
                  .replace(newScience.tail)

        val replaceAllScience: Endo[InstrumentExecutionConfig] =
          replaceScience(
            EditableSequence.gmosNorthScience,
            InstrumentExecutionConfig.gmosNorth
              .andThen(InstrumentExecutionConfig.GmosNorth.executionConfig)
          ) >>>
            replaceScience(
              EditableSequence.gmosSouthScience,
              InstrumentExecutionConfig.gmosSouth
                .andThen(InstrumentExecutionConfig.GmosSouth.executionConfig)
            ) >>>
            replaceScience(
              EditableSequence.flamingos2Science,
              InstrumentExecutionConfig.flamingos2
                .andThen(InstrumentExecutionConfig.Flamingos2.executionConfig)
            )

        val commitEdits: Callback = // TODO Invoke ODB with new sequence
          liveSequence.sequence.toOption
            .flatMap(_.toOptionView)
            .map:
              _.zoom(SequenceData.config).mod(replaceAllAcquisitions >>> replaceAllScience)
            .orEmpty

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

        def modSequence[D](
          editableOptional: Optional[EditableSequence, D]
        ): Endo[D] => Callback =
          undoCtx
            .zoom(Iso.id.some.andThen(editableOptional))
            .foldMap(_.mod)

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
                  <.span(ExploreStyles.SequenceTileTitleSide, ExploreStyles.SequenceTileTitleUndo)(
                    UndoButtons(undoCtx, size = PlSize.Mini).when(props.isEditing.get)
                  ),
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
                          onClick = props.isEditing.set(IsEditing.False) >> commitEdits,
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
            .flatMap: _ =>
              (liveSequence.visits.value, liveSequence.sequence.value.map(_.get)).tupled
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
                            modSequence(EditableSequence.gmosNorthAcquisition),
                            modSequence(EditableSequence.gmosNorthScience)
                          )
                        case ModeSignalToNoise.GmosNorthImaging(snPerFilter)          =>
                          GmosNorthImagingSequenceTable(
                            visits,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosNorthAcquisition),
                            resolveScience(config, EditableSequence.gmosNorthScience),
                            snPerFilter,
                            props.isEditing.get,
                            modSequence(EditableSequence.gmosNorthAcquisition),
                            modSequence(EditableSequence.gmosNorthScience)
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
                            modSequence(EditableSequence.gmosSouthAcquisition),
                            modSequence(EditableSequence.gmosSouthScience)
                          )
                        case ModeSignalToNoise.GmosSouthImaging(snPerFilter)          =>
                          GmosSouthImagingSequenceTable(
                            visits,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosSouthAcquisition),
                            resolveScience(config, EditableSequence.gmosSouthScience),
                            snPerFilter,
                            props.isEditing.get,
                            modSequence(EditableSequence.gmosSouthAcquisition),
                            modSequence(EditableSequence.gmosSouthScience)
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
                        modSequence(EditableSequence.flamingos2Acquisition),
                        modSequence(EditableSequence.flamingos2Science)
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
