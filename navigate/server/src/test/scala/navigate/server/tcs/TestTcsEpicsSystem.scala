// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import algebra.instances.array.given
import cats.Applicative
import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import monocle.Focus
import monocle.Lens
import mouse.all.booleanSyntaxMouse
import navigate.epics.Channel
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.TestChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.VerifiedEpics
import navigate.server.ApplyCommandResult
import navigate.server.acm.CadDirective
import navigate.server.acm.GeminiApplyCommand
import navigate.server.acm.ObserveCommand
import navigate.server.epicsdata.BinaryOnOff
import navigate.server.epicsdata.BinaryOnOffCapitalized
import navigate.server.epicsdata.BinaryYesNo
import navigate.server.tcs.TcsChannels.AdjustChannels
import navigate.server.tcs.TcsChannels.AgMechChannels
import navigate.server.tcs.TcsChannels.ChopConfigChannels
import navigate.server.tcs.TcsChannels.ChopRelativeChannels
import navigate.server.tcs.TcsChannels.EnclosureChannels
import navigate.server.tcs.TcsChannels.GuideConfigStatusChannels
import navigate.server.tcs.TcsChannels.InstrumentOffsetCommandChannels
import navigate.server.tcs.TcsChannels.M1Channels
import navigate.server.tcs.TcsChannels.M1GuideConfigChannels
import navigate.server.tcs.TcsChannels.M2BafflesChannels
import navigate.server.tcs.TcsChannels.M2GuideConfigChannels
import navigate.server.tcs.TcsChannels.MountGuideChannels
import navigate.server.tcs.TcsChannels.OffsetCommandChannels
import navigate.server.tcs.TcsChannels.OiwfsSelectChannels
import navigate.server.tcs.TcsChannels.OriginChannels
import navigate.server.tcs.TcsChannels.PointingConfigChannels
import navigate.server.tcs.TcsChannels.PointingCorrections
import navigate.server.tcs.TcsChannels.PointingModelAdjustChannels
import navigate.server.tcs.TcsChannels.ProbeChannels
import navigate.server.tcs.TcsChannels.ProbeGuideModeChannels
import navigate.server.tcs.TcsChannels.ProbeTrackingChannels
import navigate.server.tcs.TcsChannels.ProbeTrackingStateChannels
import navigate.server.tcs.TcsChannels.PwfsMechCmdChannels
import navigate.server.tcs.TcsChannels.RotatorChannels
import navigate.server.tcs.TcsChannels.SlewChannels
import navigate.server.tcs.TcsChannels.TargetChannels
import navigate.server.tcs.TcsChannels.TargetFilterChannels
import navigate.server.tcs.TcsChannels.WfsChannels
import navigate.server.tcs.TcsChannels.WfsClosedLoopChannels
import navigate.server.tcs.TcsChannels.WfsObserveChannels

import scala.concurrent.duration.FiniteDuration

object TestTcsEpicsSystem {

  class TestApplyCommand[F[_]: Applicative] extends GeminiApplyCommand[F] {
    override def post(timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult] =
      VerifiedEpics.pureF[F, F, ApplyCommandResult](ApplyCommandResult.Completed)

    override def clearIfNotBusy: VerifiedEpics[F, F, Unit] = VerifiedEpics.unit
  }

  class TestObserveCommand[F[_]: Applicative](integrating: Channel[F, BinaryYesNo])
      extends ObserveCommand[F] {
    override def post(
      typ:     ObserveCommand.CommandType,
      timeout: FiniteDuration
    ): VerifiedEpics[F, F, ApplyCommandResult] = VerifiedEpics.liftF {
      integrating
        .put((typ === ObserveCommand.CommandType.PermanentOn).fold(BinaryYesNo.Yes, BinaryYesNo.No))
        .as(ApplyCommandResult.Completed)
    }

    override def clearIfNotBusy: VerifiedEpics[F, F, Unit] = VerifiedEpics.unit
  }

  case class EnclosureChannelsState(
    ecsDomeMode:      TestChannel.State[String],
    ecsShutterMode:   TestChannel.State[String],
    ecsSlitHeight:    TestChannel.State[String],
    ecsDomeEnable:    TestChannel.State[String],
    ecsShutterEnable: TestChannel.State[String],
    ecsMoveAngle:     TestChannel.State[String],
    ecsShutterTop:    TestChannel.State[String],
    ecsShutterBottom: TestChannel.State[String],
    ecsVentGateEast:  TestChannel.State[String],
    ecsVentGateWest:  TestChannel.State[String]
  )

  case class TargetChannelsState(
    objectName:     TestChannel.State[String],
    coordSystem:    TestChannel.State[String],
    coord1:         TestChannel.State[String],
    coord2:         TestChannel.State[String],
    epoch:          TestChannel.State[String],
    equinox:        TestChannel.State[String],
    parallax:       TestChannel.State[String],
    properMotion1:  TestChannel.State[String],
    properMotion2:  TestChannel.State[String],
    radialVelocity: TestChannel.State[String],
    brightness:     TestChannel.State[String],
    ephemerisFile:  TestChannel.State[String]
  )

  object TargetChannelsState {
    val default: TargetChannelsState = TargetChannelsState(
      objectName = TestChannel.State.default,
      coordSystem = TestChannel.State.default,
      coord1 = TestChannel.State.default,
      coord2 = TestChannel.State.default,
      epoch = TestChannel.State.default,
      equinox = TestChannel.State.default,
      parallax = TestChannel.State.default,
      properMotion1 = TestChannel.State.default,
      properMotion2 = TestChannel.State.default,
      radialVelocity = TestChannel.State.default,
      brightness = TestChannel.State.default,
      ephemerisFile = TestChannel.State.default
    )
  }

  case class ProbeTrackingState(
    nodAchopA: TestChannel.State[String],
    nodAchopB: TestChannel.State[String],
    nodBchopA: TestChannel.State[String],
    nodBchopB: TestChannel.State[String]
  )

  case class ProbeState(
    parkDir: TestChannel.State[CadDirective],
    follow:  TestChannel.State[String]
  )

  case class ProbeTrackingStateState(
    nodAchopA: TestChannel.State[String],
    nodAchopB: TestChannel.State[String],
    nodBchopA: TestChannel.State[String],
    nodBchopB: TestChannel.State[String]
  )

  case class SlewChannelsState(
    zeroChopThrow:            TestChannel.State[String],
    zeroSourceOffset:         TestChannel.State[String],
    zeroSourceDiffTrack:      TestChannel.State[String],
    zeroMountOffset:          TestChannel.State[String],
    zeroMountDiffTrack:       TestChannel.State[String],
    shortcircuitTargetFilter: TestChannel.State[String],
    shortcircuitMountFilter:  TestChannel.State[String],
    resetPointing:            TestChannel.State[String],
    stopGuide:                TestChannel.State[String],
    zeroGuideOffset:          TestChannel.State[String],
    zeroInstrumentOffset:     TestChannel.State[String],
    autoparkPwfs1:            TestChannel.State[String],
    autoparkPwfs2:            TestChannel.State[String],
    autoparkOiwfs:            TestChannel.State[String],
    autoparkGems:             TestChannel.State[String],
    autoparkAowfs:            TestChannel.State[String]
  )

  case class RotatorChannelState(
    ipa:     TestChannel.State[String],
    system:  TestChannel.State[String],
    equinox: TestChannel.State[String],
    iaa:     TestChannel.State[String]
  )

  case class OriginChannelState(
    xa: TestChannel.State[String],
    ya: TestChannel.State[String],
    xb: TestChannel.State[String],
    yb: TestChannel.State[String],
    xc: TestChannel.State[String],
    yc: TestChannel.State[String]
  )

  case class WfsObserveChannelState(
    numberOfExposures: TestChannel.State[String],
    interval:          TestChannel.State[String],
    options:           TestChannel.State[String],
    label:             TestChannel.State[String],
    output:            TestChannel.State[String],
    path:              TestChannel.State[String],
    fileName:          TestChannel.State[String]
  )

  object WfsObserveChannelState {
    val default: WfsObserveChannelState = WfsObserveChannelState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class WfsClosedLoopChannelState(
    global:      TestChannel.State[String],
    average:     TestChannel.State[String],
    zernikes2m2: TestChannel.State[String],
    mult:        TestChannel.State[String]
  )

  object WfsClosedLoopChannelState {
    val default: WfsClosedLoopChannelState = WfsClosedLoopChannelState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class ProbeGuideModeState(
    state: TestChannel.State[String],
    from:  TestChannel.State[String],
    to:    TestChannel.State[String]
  )

  case class OiwfsSelectState(
    oiwfsName: TestChannel.State[String],
    output:    TestChannel.State[String]
  )

  object OiwfsSelectState {
    val default: OiwfsSelectState = OiwfsSelectState(
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  object ProbeGuideModeState {
    val default: ProbeGuideModeState = ProbeGuideModeState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class M2BafflesState(
    deployBaffle:  TestChannel.State[String],
    centralBaffle: TestChannel.State[String]
  )

  object M2BafflesState {
    val default: M2BafflesState = M2BafflesState(
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class WfsChannelState(
    observe:    WfsObserveChannelState,
    stop:       TestChannel.State[CadDirective],
    signalProc: TestChannel.State[String],
    dark:       TestChannel.State[String],
    closedLoop: WfsClosedLoopChannelState
  )

  object WfsChannelState {
    val default: WfsChannelState = WfsChannelState(
      WfsObserveChannelState.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      WfsClosedLoopChannelState.default
    )
  }

  case class PointingOffsetState(
    localCA: TestChannel.State[Double],
    localCE: TestChannel.State[Double],
    guideCA: TestChannel.State[Double],
    guideCE: TestChannel.State[Double]
  )

  object PointingOffsetState {
    val default: PointingOffsetState = PointingOffsetState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class PointingConfigState(
    name:  TestChannel.State[String],
    level: TestChannel.State[String],
    value: TestChannel.State[String]
  )

  object PointingConfigState {
    val default: PointingConfigState = PointingConfigState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class OffsetCommandState(
    vt:    TestChannel.State[String],
    index: TestChannel.State[String]
  )

  object OffsetCommandState {
    val default: OffsetCommandState = OffsetCommandState(
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class InstrumentOffsetCommandState(
    offsetX: TestChannel.State[String],
    offsetY: TestChannel.State[String]
  )

  object InstrumentOffsetCommandState {
    val default: InstrumentOffsetCommandState = InstrumentOffsetCommandState(
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class ChopConfigState(
    typ:       TestChannel.State[String],
    source:    TestChannel.State[String],
    frequency: TestChannel.State[String],
    dutyCycle: TestChannel.State[String]
  )

  object ChopConfigState {
    val default: ChopConfigState = ChopConfigState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class ChopRelativeState(
    thrw:    TestChannel.State[String],
    angle:   TestChannel.State[String],
    system:  TestChannel.State[String],
    equinox: TestChannel.State[String]
  )

  object ChopRelativeState {
    val default: ChopRelativeState = ChopRelativeState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  case class State(
    telltale:             TestChannel.State[String],
    telescopeParkDir:     TestChannel.State[CadDirective],
    mountFollow:          TestChannel.State[String],
    rotStopBrake:         TestChannel.State[String],
    rotParkDir:           TestChannel.State[CadDirective],
    rotFollow:            TestChannel.State[String],
    rotMoveAngle:         TestChannel.State[String],
    enclosure:            EnclosureChannelsState,
    sourceA:              TargetChannelsState,
    pwfs1Target:          TargetChannelsState,
    pwfs2Target:          TargetChannelsState,
    oiwfsTarget:          TargetChannelsState,
    wavelSourceA:         TestChannel.State[String],
    wavelPwfs1:           TestChannel.State[String],
    wavelPwfs2:           TestChannel.State[String],
    wavelOiwfs:           TestChannel.State[String],
    slew:                 SlewChannelsState,
    rotator:              RotatorChannelState,
    origin:               OriginChannelState,
    focusOffset:          TestChannel.State[String],
    pwfs1Tracking:        ProbeTrackingState,
    pwfs1Probe:           ProbeState,
    pwfs2Tracking:        ProbeTrackingState,
    pwfs2Probe:           ProbeState,
    oiwfsTracking:        ProbeTrackingState,
    oiwfsProbe:           ProbeState,
    m1Guide:              TestChannel.State[String],
    m1GuideConfig:        M1GuideConfigState,
    m2Guide:              TestChannel.State[String],
    m2GuideMode:          TestChannel.State[String],
    m2GuideConfig:        M2GuideConfigState,
    m2GuideReset:         TestChannel.State[CadDirective],
    m2Follow:             TestChannel.State[String],
    mountGuide:           MountGuideState,
    pwfs1:                WfsChannelState,
    pwfs2:                WfsChannelState,
    oiwfs:                WfsChannelState,
    guideStatus:          GuideConfigState,
    probeGuideMode:       ProbeGuideModeState,
    oiwfsSelect:          OiwfsSelectState,
    m2Baffles:            M2BafflesState,
    hrwfsMech:            AgMechState,
    scienceFoldMech:      AgMechState,
    aoFoldMech:           AgMechState,
    m1Cmds:               M1CommandsState,
    nodState:             TestChannel.State[String],
    pwfs1TrackingState:   ProbeTrackingStateState,
    pwfs2TrackingState:   ProbeTrackingStateState,
    oiwfsTrackingState:   ProbeTrackingStateState,
    targetAdjust:         AdjustCommandState,
    targetOffsetAbsorb:   OffsetCommandState,
    targetOffsetClear:    OffsetCommandState,
    originAdjust:         AdjustCommandState,
    originOffsetAbsorb:   OffsetCommandState,
    originOffsetClear:    OffsetCommandState,
    pointingAdjust:       PointingAdjustCommandState,
    inPosition:           TestChannel.State[String],
    targetFilter:         TargetFilterCommandState,
    sourceATargetReadout: TestChannel.State[Array[Double]],
    pwfs1TargetReadout:   TestChannel.State[Array[Double]],
    pwfs2TargetReadout:   TestChannel.State[Array[Double]],
    oiwfsTargetReadout:   TestChannel.State[Array[Double]],
    pointingOffsetState:  PointingOffsetState,
    pointingConfig:       PointingConfigState,
    absorbGuideDirState:  TestChannel.State[CadDirective],
    zeroGuideDirState:    TestChannel.State[CadDirective],
    instrumentOffset:     InstrumentOffsetCommandState,
    azimuthWrap:          TestChannel.State[String],
    rotatorWrap:          TestChannel.State[String],
    zeroRotatorGuide:     TestChannel.State[CadDirective],
    p1Filter:             TestChannel.State[String],
    p1FieldStop:          TestChannel.State[String],
    p2Filter:             TestChannel.State[String],
    p2FieldStop:          TestChannel.State[String],
    chopConfig:           ChopConfigState,
    chopRelative:         ChopRelativeState
  )

  val defaultState: State = State(
    telltale = TestChannel.State.default,
    telescopeParkDir = TestChannel.State.default,
    mountFollow = TestChannel.State.default,
    rotStopBrake = TestChannel.State.default,
    rotParkDir = TestChannel.State.default,
    rotFollow = TestChannel.State.default,
    rotMoveAngle = TestChannel.State.default,
    enclosure = EnclosureChannelsState(
      ecsDomeMode = TestChannel.State.default,
      ecsShutterMode = TestChannel.State.default,
      ecsSlitHeight = TestChannel.State.default,
      ecsDomeEnable = TestChannel.State.default,
      ecsShutterEnable = TestChannel.State.default,
      ecsMoveAngle = TestChannel.State.default,
      ecsShutterTop = TestChannel.State.default,
      ecsShutterBottom = TestChannel.State.default,
      ecsVentGateEast = TestChannel.State.default,
      ecsVentGateWest = TestChannel.State.default
    ),
    sourceA = TargetChannelsState.default,
    pwfs1Target = TargetChannelsState.default,
    pwfs2Target = TargetChannelsState.default,
    oiwfsTarget = TargetChannelsState.default,
    wavelSourceA = TestChannel.State.default,
    wavelPwfs1 = TestChannel.State.default,
    wavelPwfs2 = TestChannel.State.default,
    wavelOiwfs = TestChannel.State.default,
    slew = SlewChannelsState(
      zeroChopThrow = TestChannel.State.default,
      zeroSourceOffset = TestChannel.State.default,
      zeroSourceDiffTrack = TestChannel.State.default,
      zeroMountOffset = TestChannel.State.default,
      zeroMountDiffTrack = TestChannel.State.default,
      shortcircuitTargetFilter = TestChannel.State.default,
      shortcircuitMountFilter = TestChannel.State.default,
      resetPointing = TestChannel.State.default,
      stopGuide = TestChannel.State.default,
      zeroGuideOffset = TestChannel.State.default,
      zeroInstrumentOffset = TestChannel.State.default,
      autoparkPwfs1 = TestChannel.State.default,
      autoparkPwfs2 = TestChannel.State.default,
      autoparkOiwfs = TestChannel.State.default,
      autoparkGems = TestChannel.State.default,
      autoparkAowfs = TestChannel.State.default
    ),
    rotator = RotatorChannelState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    ),
    origin = OriginChannelState(
      xa = TestChannel.State.default,
      ya = TestChannel.State.default,
      xb = TestChannel.State.default,
      yb = TestChannel.State.default,
      xc = TestChannel.State.default,
      yc = TestChannel.State.default
    ),
    focusOffset = TestChannel.State.default,
    pwfs1Tracking = ProbeTrackingState(
      nodAchopA = TestChannel.State.of("Off"),
      nodAchopB = TestChannel.State.of("Off"),
      nodBchopA = TestChannel.State.of("Off"),
      nodBchopB = TestChannel.State.of("Off")
    ),
    pwfs1Probe =
      ProbeState(parkDir = TestChannel.State.default, follow = TestChannel.State.default),
    pwfs2Tracking = ProbeTrackingState(
      nodAchopA = TestChannel.State.of("Off"),
      nodAchopB = TestChannel.State.of("Off"),
      nodBchopA = TestChannel.State.of("Off"),
      nodBchopB = TestChannel.State.of("Off")
    ),
    pwfs2Probe =
      ProbeState(parkDir = TestChannel.State.default, follow = TestChannel.State.default),
    oiwfsTracking = ProbeTrackingState(
      nodAchopA = TestChannel.State.of("Off"),
      nodAchopB = TestChannel.State.of("Off"),
      nodBchopA = TestChannel.State.of("Off"),
      nodBchopB = TestChannel.State.of("Off")
    ),
    oiwfsProbe =
      ProbeState(parkDir = TestChannel.State.default, follow = TestChannel.State.default),
    m1Guide = TestChannel.State.default,
    m1GuideConfig = M1GuideConfigState.default,
    m2Guide = TestChannel.State.default,
    m2GuideMode = TestChannel.State.default,
    m2GuideConfig = M2GuideConfigState.default,
    m2GuideReset = TestChannel.State.default,
    m2Follow = TestChannel.State.default,
    mountGuide = MountGuideState.default,
    pwfs1 = WfsChannelState.default,
    pwfs2 = WfsChannelState.default,
    oiwfs = WfsChannelState.default,
    guideStatus = GuideConfigState.default,
    probeGuideMode = ProbeGuideModeState.default,
    oiwfsSelect = OiwfsSelectState.default,
    m2Baffles = M2BafflesState.default,
    hrwfsMech = AgMechState.default,
    scienceFoldMech = AgMechState.default,
    aoFoldMech = AgMechState.default,
    m1Cmds = M1CommandsState.default,
    nodState = TestChannel.State.of("A"),
    pwfs1TrackingState = ProbeTrackingStateState(
      nodAchopA = TestChannel.State.of("Off"),
      nodAchopB = TestChannel.State.of("Off"),
      nodBchopA = TestChannel.State.of("Off"),
      nodBchopB = TestChannel.State.of("Off")
    ),
    pwfs2TrackingState = ProbeTrackingStateState(
      nodAchopA = TestChannel.State.of("Off"),
      nodAchopB = TestChannel.State.of("Off"),
      nodBchopA = TestChannel.State.of("Off"),
      nodBchopB = TestChannel.State.of("Off")
    ),
    oiwfsTrackingState = ProbeTrackingStateState(
      nodAchopA = TestChannel.State.of("Off"),
      nodAchopB = TestChannel.State.of("Off"),
      nodBchopA = TestChannel.State.of("Off"),
      nodBchopB = TestChannel.State.of("Off")
    ),
    targetAdjust = AdjustCommandState.default,
    targetOffsetAbsorb = OffsetCommandState.default,
    targetOffsetClear = OffsetCommandState.default,
    originAdjust = AdjustCommandState.default,
    originOffsetAbsorb = OffsetCommandState.default,
    originOffsetClear = OffsetCommandState.default,
    pointingAdjust = PointingAdjustCommandState.default,
    inPosition = TestChannel.State.of("FALSE"),
    targetFilter = TargetFilterCommandState.default,
    sourceATargetReadout = TestChannel.State.of[Array[Double]](Array.fill(8)(0.0)),
    pwfs1TargetReadout = TestChannel.State.of[Array[Double]](Array.fill(8)(0.0)),
    pwfs2TargetReadout = TestChannel.State.of[Array[Double]](Array.fill(8)(0.0)),
    oiwfsTargetReadout = TestChannel.State.of[Array[Double]](Array.fill(8)(0.0)),
    pointingOffsetState = PointingOffsetState.default,
    pointingConfig = PointingConfigState.default,
    absorbGuideDirState = TestChannel.State.default,
    zeroGuideDirState = TestChannel.State.default,
    instrumentOffset = InstrumentOffsetCommandState.default,
    azimuthWrap = TestChannel.State.default,
    rotatorWrap = TestChannel.State.default,
    zeroRotatorGuide = TestChannel.State.default,
    p1Filter = TestChannel.State.default,
    p1FieldStop = TestChannel.State.default,
    p2Filter = TestChannel.State.default,
    p2FieldStop = TestChannel.State.default,
    chopConfig = ChopConfigState.default,
    chopRelative = ChopRelativeState.default
  )

  def buildEnclosureChannels[F[_]: Temporal](s: Ref[F, State]): EnclosureChannels[F] =
    EnclosureChannels[F](
      ecsDomeMode = new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsDomeMode)),
      ecsShutterMode =
        new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsShutterMode)),
      ecsSlitHeight = new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsSlitHeight)),
      ecsDomeEnable = new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsDomeEnable)),
      ecsShutterEnable =
        new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsShutterEnable)),
      ecsMoveAngle = new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsMoveAngle)),
      ecsShutterTop = new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsShutterTop)),
      ecsShutterBottom =
        new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsShutterBottom)),
      ecsVentGateEast =
        new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsVentGateEast)),
      ecsVentGateWest =
        new TestChannel[F, State, String](s, Focus[State](_.enclosure.ecsVentGateWest))
    )

  def buildTargetChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, TargetChannelsState]
  ): TargetChannels[F] =
    TargetChannels[F](
      objectName =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.objectName))),
      coordSystem =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.coordSystem))),
      coord1 =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.coord1))),
      coord2 =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.coord2))),
      epoch = new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.epoch))),
      equinox =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.equinox))),
      parallax =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.parallax))),
      properMotion1 =
        new TestChannel[F, State, String](s,
                                          l.andThen(Focus[TargetChannelsState](_.properMotion1))
        ),
      properMotion2 =
        new TestChannel[F, State, String](s,
                                          l.andThen(Focus[TargetChannelsState](_.properMotion2))
        ),
      radialVelocity =
        new TestChannel[F, State, String](s,
                                          l.andThen(Focus[TargetChannelsState](_.radialVelocity))
        ),
      brightness =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.brightness))),
      ephemerisFile =
        new TestChannel[F, State, String](s, l.andThen(Focus[TargetChannelsState](_.ephemerisFile)))
    )

  def buildSlewChannels[F[_]: Temporal](
    s: Ref[F, State]
  ): SlewChannels[F] =
    SlewChannels(
      zeroChopThrow = new TestChannel[F, State, String](s, Focus[State](_.slew.zeroChopThrow)),
      zeroSourceOffset =
        new TestChannel[F, State, String](s, Focus[State](_.slew.zeroSourceOffset)),
      zeroSourceDiffTrack = new TestChannel[F, State, String](
        s,
        Focus[State](_.slew.zeroSourceDiffTrack)
      ),
      zeroMountOffset = new TestChannel[F, State, String](
        s,
        Focus[State](_.slew.zeroMountOffset)
      ),
      zeroMountDiffTrack = new TestChannel[F, State, String](
        s,
        Focus[State](_.slew.zeroMountDiffTrack)
      ),
      shortcircuitTargetFilter = new TestChannel[F, State, String](
        s,
        Focus[State](_.slew.shortcircuitTargetFilter)
      ),
      shortcircuitMountFilter = new TestChannel[F, State, String](
        s,
        Focus[State](_.slew.shortcircuitMountFilter)
      ),
      resetPointing = new TestChannel[F, State, String](s, Focus[State](_.slew.resetPointing)),
      stopGuide = new TestChannel[F, State, String](s, Focus[State](_.slew.stopGuide)),
      zeroGuideOffset = new TestChannel[F, State, String](s, Focus[State](_.slew.zeroGuideOffset)),
      zeroInstrumentOffset = new TestChannel[F, State, String](
        s,
        Focus[State](_.slew.zeroInstrumentOffset)
      ),
      autoparkPwfs1 = new TestChannel[F, State, String](s, Focus[State](_.slew.autoparkPwfs1)),
      autoparkPwfs2 = new TestChannel[F, State, String](s, Focus[State](_.slew.autoparkPwfs2)),
      autoparkOiwfs = new TestChannel[F, State, String](s, Focus[State](_.slew.autoparkOiwfs)),
      autoparkGems = new TestChannel[F, State, String](s, Focus[State](_.slew.autoparkGems)),
      autoparkAowfs = new TestChannel[F, State, String](s, Focus[State](_.slew.autoparkAowfs))
    )

  def buildRotatorChannels[F[_]: Temporal](s: Ref[F, State]): RotatorChannels[F] =
    RotatorChannels(
      new TestChannel[F, State, String](s, Focus[State](_.rotator.ipa)),
      new TestChannel[F, State, String](s, Focus[State](_.rotator.system)),
      new TestChannel[F, State, String](s, Focus[State](_.rotator.equinox)),
      new TestChannel[F, State, String](s, Focus[State](_.rotator.iaa))
    )

  def buildOriginChannels[F[_]: Temporal](s: Ref[F, State]): OriginChannels[F] =
    OriginChannels(
      new TestChannel[F, State, String](s, Focus[State](_.origin.xa)),
      new TestChannel[F, State, String](s, Focus[State](_.origin.ya)),
      new TestChannel[F, State, String](s, Focus[State](_.origin.xb)),
      new TestChannel[F, State, String](s, Focus[State](_.origin.yb)),
      new TestChannel[F, State, String](s, Focus[State](_.origin.xc)),
      new TestChannel[F, State, String](s, Focus[State](_.origin.yc))
    )

  def buildProbeTrackingChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, ProbeTrackingState]
  ): ProbeTrackingChannels[F] = ProbeTrackingChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingState](_.nodAchopA))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingState](_.nodAchopB))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingState](_.nodBchopA))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingState](_.nodBchopB)))
  )

  def buildProbeTrackingStateChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, ProbeTrackingStateState]
  ): ProbeTrackingStateChannels[F] = ProbeTrackingStateChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingStateState](_.nodAchopA))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingStateState](_.nodAchopB))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingStateState](_.nodBchopA))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeTrackingStateState](_.nodBchopB)))
  )

  def buildProbeChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, ProbeState]
  ): ProbeChannels[F] = ProbeChannels(
    new TestChannel[F, State, CadDirective](s, l.andThen(Focus[ProbeState](_.parkDir))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeState](_.follow)))
  )

  case class M1GuideConfigState(
    weighting: TestChannel.State[String],
    source:    TestChannel.State[String],
    frames:    TestChannel.State[String],
    filename:  TestChannel.State[String]
  )

  object M1GuideConfigState {
    val default: M1GuideConfigState = M1GuideConfigState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  def buildM1GuideConfigChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, M1GuideConfigState]
  ): M1GuideConfigChannels[F] = M1GuideConfigChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[M1GuideConfigState](_.weighting))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M1GuideConfigState](_.source))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M1GuideConfigState](_.frames))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M1GuideConfigState](_.filename)))
  )

  case class GuideConfigState(
    pwfs1Integrating:  TestChannel.State[BinaryYesNo],
    pwfs2Integrating:  TestChannel.State[BinaryYesNo],
    oiwfsIntegrating:  TestChannel.State[BinaryYesNo],
    m2State:           TestChannel.State[BinaryOnOff],
    absorbTipTilt:     TestChannel.State[Int],
    m2ComaCorrection:  TestChannel.State[BinaryOnOff],
    m1State:           TestChannel.State[BinaryOnOff],
    m1Source:          TestChannel.State[String],
    p1ProbeGuide:      TestChannel.State[Double],
    p2ProbeGuide:      TestChannel.State[Double],
    oiProbeGuide:      TestChannel.State[Double],
    p1ProbeGuided:     TestChannel.State[Double],
    p2ProbeGuided:     TestChannel.State[Double],
    oiProbeGuided:     TestChannel.State[Double],
    mountP1Weight:     TestChannel.State[Double],
    mountP2Weight:     TestChannel.State[Double],
    m2P1Guide:         TestChannel.State[String],
    m2P2Guide:         TestChannel.State[String],
    m2OiGuide:         TestChannel.State[String],
    m2AoGuide:         TestChannel.State[String],
    probeGuideEnabled: TestChannel.State[String],
    probeGuideSource:  TestChannel.State[String],
    probeGuideSink:    TestChannel.State[String]
  )

  object GuideConfigState {
    val default: GuideConfigState = GuideConfigState(
      TestChannel.State.of(BinaryYesNo.No),
      TestChannel.State.of(BinaryYesNo.No),
      TestChannel.State.of(BinaryYesNo.No),
      TestChannel.State.of(BinaryOnOff.Off),
      TestChannel.State.of(0),
      TestChannel.State.of(BinaryOnOff.Off),
      TestChannel.State.of(BinaryOnOff.Off),
      TestChannel.State.of(""),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of(0.0),
      TestChannel.State.of("OFF"),
      TestChannel.State.of("OFF"),
      TestChannel.State.of("OFF"),
      TestChannel.State.of("OFF"),
      TestChannel.State.of("Off"),
      TestChannel.State.of(""),
      TestChannel.State.of("")
    )
  }

  def buildGuideStateChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, GuideConfigState]
  ): GuideConfigStatusChannels[F] = GuideConfigStatusChannels(
    new TestChannel[F, State, BinaryYesNo](s,
                                           l.andThen(Focus[GuideConfigState](_.pwfs1Integrating))
    ),
    new TestChannel[F, State, BinaryYesNo](s,
                                           l.andThen(Focus[GuideConfigState](_.pwfs2Integrating))
    ),
    new TestChannel[F, State, BinaryYesNo](s,
                                           l.andThen(Focus[GuideConfigState](_.oiwfsIntegrating))
    ),
    new TestChannel[F, State, BinaryOnOff](s, l.andThen(Focus[GuideConfigState](_.m2State))),
    new TestChannel[F, State, Int](s, l.andThen(Focus[GuideConfigState](_.absorbTipTilt))),
    new TestChannel[F, State, BinaryOnOff](s,
                                           l.andThen(Focus[GuideConfigState](_.m2ComaCorrection))
    ),
    new TestChannel[F, State, BinaryOnOff](s, l.andThen(Focus[GuideConfigState](_.m1State))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.m1Source))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.p1ProbeGuide))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.p2ProbeGuide))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.oiProbeGuide))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.p1ProbeGuided))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.p2ProbeGuided))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.oiProbeGuided))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.mountP1Weight))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[GuideConfigState](_.mountP2Weight))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.m2P1Guide))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.m2P2Guide))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.m2OiGuide))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.m2AoGuide))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.probeGuideEnabled))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.probeGuideSource))),
    new TestChannel[F, State, String](s, l.andThen(Focus[GuideConfigState](_.probeGuideSink)))
  )

  case class M2GuideConfigState(
    source:     TestChannel.State[String],
    samplefreq: TestChannel.State[String],
    filter:     TestChannel.State[String],
    freq1:      TestChannel.State[String],
    freq2:      TestChannel.State[String],
    beam:       TestChannel.State[String],
    reset:      TestChannel.State[String]
  )

  object M2GuideConfigState {
    val default: M2GuideConfigState = M2GuideConfigState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  def buildM2GuideConfigChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, M2GuideConfigState]
  ): M2GuideConfigChannels[F] = M2GuideConfigChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.source))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.samplefreq))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.filter))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.freq1))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.freq2))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.beam))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2GuideConfigState](_.reset)))
  )

  case class MountGuideState(
    mode:     TestChannel.State[String],
    source:   TestChannel.State[String],
    p1weight: TestChannel.State[String],
    p2weight: TestChannel.State[String]
  )

  object MountGuideState {
    val default: MountGuideState = MountGuideState(
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default,
      TestChannel.State.default
    )
  }

  def buildMountGuideChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, MountGuideState]
  ): MountGuideChannels[F] = MountGuideChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[MountGuideState](_.mode))),
    new TestChannel[F, State, String](s, l.andThen(Focus[MountGuideState](_.source))),
    new TestChannel[F, State, String](s, l.andThen(Focus[MountGuideState](_.p1weight))),
    new TestChannel[F, State, String](s, l.andThen(Focus[MountGuideState](_.p2weight)))
  )

  def buildWfsChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, WfsChannelState]
  ): WfsChannels[F] = WfsChannels(
    WfsObserveChannels(
      new TestChannel[F, State, String](
        s,
        l.andThen(Focus[WfsChannelState](_.observe.numberOfExposures))
      ),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.observe.interval))),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.observe.options))),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.observe.label))),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.observe.output))),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.observe.path))),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.observe.fileName)))
    ),
    new TestChannel[F, State, CadDirective](s, l.andThen(Focus[WfsChannelState](_.stop))),
    new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.signalProc))),
    new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.dark))),
    WfsClosedLoopChannels(
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.closedLoop.global))),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.closedLoop.average))),
      new TestChannel[F, State, String](s,
                                        l.andThen(Focus[WfsChannelState](_.closedLoop.zernikes2m2))
      ),
      new TestChannel[F, State, String](s, l.andThen(Focus[WfsChannelState](_.closedLoop.mult)))
    )
  )

  def buildGuideModeChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, ProbeGuideModeState]
  ): ProbeGuideModeChannels[F] = ProbeGuideModeChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeGuideModeState](_.state))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeGuideModeState](_.from))),
    new TestChannel[F, State, String](s, l.andThen(Focus[ProbeGuideModeState](_.to)))
  )

  def buildOiwfsSelectChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, OiwfsSelectState]
  ): OiwfsSelectChannels[F] = OiwfsSelectChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[OiwfsSelectState](_.oiwfsName))),
    new TestChannel[F, State, String](s, l.andThen(Focus[OiwfsSelectState](_.output)))
  )

  def buildM2BafflesChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, M2BafflesState]
  ): M2BafflesChannels[F] = M2BafflesChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[M2BafflesState](_.deployBaffle))),
    new TestChannel[F, State, String](s, l.andThen(Focus[M2BafflesState](_.centralBaffle)))
  )

  def buildAdjustChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, AdjustCommandState]
  ): AdjustChannels[F] = AdjustChannels(
    new TestChannel[F, State, String](s, l.andThen(Focus[AdjustCommandState](_.frame))),
    new TestChannel[F, State, String](s, l.andThen(Focus[AdjustCommandState](_.size))),
    new TestChannel[F, State, String](s, l.andThen(Focus[AdjustCommandState](_.angle))),
    new TestChannel[F, State, String](s, l.andThen(Focus[AdjustCommandState](_.vt)))
  )

  case class AgMechState(
    parkDir:  TestChannel.State[CadDirective],
    position: TestChannel.State[String]
  )

  object AgMechState {
    val default: AgMechState = AgMechState(
      TestChannel.State.default[CadDirective],
      TestChannel.State.default[String]
    )
  }

  case class M1CommandsState(
    telltale:      TestChannel.State[String],
    park:          TestChannel.State[String],
    figUpdates:    TestChannel.State[String],
    zero:          TestChannel.State[String],
    loadModelFile: TestChannel.State[String],
    saveModelFile: TestChannel.State[String],
    aoEnable:      TestChannel.State[BinaryOnOffCapitalized]
  )

  case class AdjustCommandState(
    frame: TestChannel.State[String],
    size:  TestChannel.State[String],
    angle: TestChannel.State[String],
    vt:    TestChannel.State[String]
  )

  object AdjustCommandState {
    val default: AdjustCommandState = AdjustCommandState(
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String]
    )
  }

  case class PointingAdjustCommandState(
    frame: TestChannel.State[String],
    size:  TestChannel.State[String],
    angle: TestChannel.State[String]
  )

  object PointingAdjustCommandState {
    val default: PointingAdjustCommandState = PointingAdjustCommandState(
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String]
    )
  }

  case class TargetFilterCommandState(
    bandwidth:    TestChannel.State[String],
    maxVelocity:  TestChannel.State[String],
    grabRadius:   TestChannel.State[String],
    shortcircuit: TestChannel.State[String]
  )

  object TargetFilterCommandState {
    val default: TargetFilterCommandState = TargetFilterCommandState(
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String]
    )
  }

  object M1CommandsState {
    val default: M1CommandsState = M1CommandsState(
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[String],
      TestChannel.State.default[BinaryOnOffCapitalized]
    )
  }

  def buildM1Channels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, M1CommandsState]
  ): M1Channels[F] = M1Channels[F](
    telltale = TelltaleChannel[F](
      "M1",
      new TestChannel[F, State, String](s, l.andThen(Focus[M1CommandsState](_.telltale)))
    ),
    park = new TestChannel[F, State, String](s, l.andThen(Focus[M1CommandsState](_.park))),
    figUpdates =
      new TestChannel[F, State, String](s, l.andThen(Focus[M1CommandsState](_.figUpdates))),
    zero = new TestChannel[F, State, String](s, l.andThen(Focus[M1CommandsState](_.zero))),
    loadModelFile =
      new TestChannel[F, State, String](s, l.andThen(Focus[M1CommandsState](_.loadModelFile))),
    saveModelFile =
      new TestChannel[F, State, String](s, l.andThen(Focus[M1CommandsState](_.saveModelFile))),
    aoEnable = new TestChannel[F, State, BinaryOnOffCapitalized](
      s,
      l.andThen(Focus[M1CommandsState](_.aoEnable))
    )
  )

  def buildAgMechChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, AgMechState]
  ): AgMechChannels[F] = AgMechChannels[F](
    new TestChannel[F, State, CadDirective](s, l.andThen(Focus[AgMechState](_.parkDir))),
    new TestChannel[F, State, String](s, l.andThen(Focus[AgMechState](_.position)))
  )

  def buildPointingAdjustChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, PointingAdjustCommandState]
  ): PointingModelAdjustChannels[F] = PointingModelAdjustChannels[F](
    new TestChannel[F, State, String](s, l.andThen(Focus[PointingAdjustCommandState](_.frame))),
    new TestChannel[F, State, String](s, l.andThen(Focus[PointingAdjustCommandState](_.size))),
    new TestChannel[F, State, String](s, l.andThen(Focus[PointingAdjustCommandState](_.angle)))
  )

  def buildTargetFilterChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, TargetFilterCommandState]
  ): TargetFilterChannels[F] = TargetFilterChannels[F](
    new TestChannel[F, State, String](s, l.andThen(Focus[TargetFilterCommandState](_.bandwidth))),
    new TestChannel[F, State, String](s, l.andThen(Focus[TargetFilterCommandState](_.maxVelocity))),
    new TestChannel[F, State, String](s, l.andThen(Focus[TargetFilterCommandState](_.grabRadius))),
    new TestChannel[F, State, String](s, l.andThen(Focus[TargetFilterCommandState](_.shortcircuit)))
  )

  def buildPointingOffsetStateChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, PointingOffsetState]
  ): PointingCorrections[F] = PointingCorrections[F](
    new TestChannel[F, State, Double](s, l.andThen(Focus[PointingOffsetState](_.localCA))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[PointingOffsetState](_.localCE))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[PointingOffsetState](_.guideCA))),
    new TestChannel[F, State, Double](s, l.andThen(Focus[PointingOffsetState](_.guideCE)))
  )

  def buildPointingConfigChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, PointingConfigState]
  ): PointingConfigChannels[F] = PointingConfigChannels[F](
    new TestChannel[F, State, String](s, l.andThen(Focus[PointingConfigState](_.name))),
    new TestChannel[F, State, String](s, l.andThen(Focus[PointingConfigState](_.level))),
    new TestChannel[F, State, String](s, l.andThen(Focus[PointingConfigState](_.value)))
  )

  def buildOffsetCommandChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, OffsetCommandState]
  ): OffsetCommandChannels[F] = OffsetCommandChannels[F](
    new TestChannel[F, State, String](s, l.andThen(Focus[OffsetCommandState](_.vt))),
    new TestChannel[F, State, String](s, l.andThen(Focus[OffsetCommandState](_.index)))
  )

  def buildInstrumentOffsetCommandChannels[F[_]: Temporal](
    s: Ref[F, State],
    l: Lens[State, InstrumentOffsetCommandState]
  ): InstrumentOffsetCommandChannels[F] =
    InstrumentOffsetCommandChannels[F](
      new TestChannel[F, State, String](s,
                                        l.andThen(Focus[InstrumentOffsetCommandState](_.offsetX))
      ),
      new TestChannel[F, State, String](s,
                                        l.andThen(Focus[InstrumentOffsetCommandState](_.offsetY))
      )
    )

  def buildChannels[F[_]: Temporal](s: Ref[F, State]): TcsChannels[F] =
    TcsChannels(
      telltale =
        TelltaleChannel[F]("TCS", new TestChannel[F, State, String](s, Focus[State](_.telltale))),
      telescopeParkDir =
        new TestChannel[F, State, CadDirective](s, Focus[State](_.telescopeParkDir)),
      mountFollow = new TestChannel[F, State, String](s, Focus[State](_.mountFollow)),
      rotStopBrake = new TestChannel[F, State, String](s, Focus[State](_.rotStopBrake)),
      rotParkDir = new TestChannel[F, State, CadDirective](s, Focus[State](_.rotParkDir)),
      rotFollow = new TestChannel[F, State, String](s, Focus[State](_.rotFollow)),
      rotMoveAngle = new TestChannel[F, State, String](s, Focus[State](_.rotMoveAngle)),
      enclosure = buildEnclosureChannels(s),
      sourceA = buildTargetChannels(s, Focus[State](_.sourceA)),
      pwfs1Target = buildTargetChannels(s, Focus[State](_.pwfs1Target)),
      pwfs2Target = buildTargetChannels(s, Focus[State](_.pwfs2Target)),
      oiwfsTarget = buildTargetChannels(s, Focus[State](_.oiwfsTarget)),
      wavelSourceA = new TestChannel[F, State, String](s, Focus[State](_.wavelSourceA)),
      wavelPwfs1 = new TestChannel[F, State, String](s, Focus[State](_.wavelPwfs1)),
      wavelPwfs2 = new TestChannel[F, State, String](s, Focus[State](_.wavelPwfs2)),
      wavelOiwfs = new TestChannel[F, State, String](s, Focus[State](_.wavelOiwfs)),
      slew = buildSlewChannels(s),
      rotator = buildRotatorChannels(s),
      origin = buildOriginChannels(s),
      focusOffset = new TestChannel[F, State, String](s, Focus[State](_.focusOffset)),
      p1ProbeTracking = buildProbeTrackingChannels(s, Focus[State](_.pwfs1Tracking)),
      p1Probe = buildProbeChannels(s, Focus[State](_.pwfs1Probe)),
      p2ProbeTracking = buildProbeTrackingChannels(s, Focus[State](_.pwfs2Tracking)),
      p2Probe = buildProbeChannels(s, Focus[State](_.pwfs2Probe)),
      oiProbeTracking = buildProbeTrackingChannels(s, Focus[State](_.oiwfsTracking)),
      oiProbe = buildProbeChannels(s, Focus[State](_.oiwfsProbe)),
      m1Guide = new TestChannel[F, State, String](s, Focus[State](_.m1Guide)),
      m1GuideConfig = buildM1GuideConfigChannels(s, Focus[State](_.m1GuideConfig)),
      m2Guide = new TestChannel[F, State, String](s, Focus[State](_.m2Guide)),
      m2GuideMode = new TestChannel[F, State, String](s, Focus[State](_.m2GuideMode)),
      m2GuideConfig = buildM2GuideConfigChannels(s, Focus[State](_.m2GuideConfig)),
      m2GuideReset = new TestChannel[F, State, CadDirective](s, Focus[State](_.m2GuideReset)),
      m2Follow = new TestChannel[F, State, String](s, Focus[State](_.m2Follow)),
      mountGuide = buildMountGuideChannels(s, Focus[State](_.mountGuide)),
      pwfs1 = buildWfsChannels(s, Focus[State](_.pwfs1)),
      pwfs2 = buildWfsChannels(s, Focus[State](_.pwfs2)),
      oiwfs = buildWfsChannels(s, Focus[State](_.oiwfs)),
      guide = buildGuideStateChannels(s, Focus[State](_.guideStatus)),
      probeGuideMode = buildGuideModeChannels(s, Focus[State](_.probeGuideMode)),
      oiwfsSelect = buildOiwfsSelectChannels(s, Focus[State](_.oiwfsSelect)),
      m2Baffles = buildM2BafflesChannels(s, Focus[State](_.m2Baffles)),
      hrwfsMech = buildAgMechChannels(s, Focus[State](_.hrwfsMech)),
      scienceFoldMech = buildAgMechChannels(s, Focus[State](_.scienceFoldMech)),
      aoFoldMech = buildAgMechChannels(s, Focus[State](_.aoFoldMech)),
      m1Channels = buildM1Channels(s, Focus[State](_.m1Cmds)),
      nodState = new TestChannel[F, State, String](s, Focus[State](_.nodState)),
      p1ProbeTrackingState = buildProbeTrackingStateChannels(s, Focus[State](_.pwfs1TrackingState)),
      p2ProbeTrackingState = buildProbeTrackingStateChannels(s, Focus[State](_.pwfs2TrackingState)),
      oiProbeTrackingState = buildProbeTrackingStateChannels(s, Focus[State](_.oiwfsTrackingState)),
      targetAdjust = buildAdjustChannels(s, Focus[State](_.targetAdjust)),
      targetOffsetAbsorb = buildOffsetCommandChannels(s, Focus[State](_.targetOffsetAbsorb)),
      targetOffsetClear = buildOffsetCommandChannels(s, Focus[State](_.targetOffsetClear)),
      originAdjust = buildAdjustChannels(s, Focus[State](_.originAdjust)),
      originOffsetAbsorb = buildOffsetCommandChannels(s, Focus[State](_.originOffsetAbsorb)),
      originOffsetClear = buildOffsetCommandChannels(s, Focus[State](_.originOffsetClear)),
      pointingAdjust = buildPointingAdjustChannels(s, Focus[State](_.pointingAdjust)),
      inPosition = new TestChannel[F, State, String](s, Focus[State](_.inPosition)),
      targetFilter = buildTargetFilterChannels(s, Focus[State](_.targetFilter)),
      sourceATargetReadout =
        new TestChannel[F, State, Array[Double]](s, Focus[State](_.sourceATargetReadout)),
      pwfs1TargetReadout =
        new TestChannel[F, State, Array[Double]](s, Focus[State](_.pwfs1TargetReadout)),
      pwfs2TargetReadout =
        new TestChannel[F, State, Array[Double]](s, Focus[State](_.pwfs2TargetReadout)),
      oiwfsTargetReadout =
        new TestChannel[F, State, Array[Double]](s, Focus[State](_.oiwfsTargetReadout)),
      pointingAdjustmentState =
        buildPointingOffsetStateChannels(s, Focus[State](_.pointingOffsetState)),
      pointingConfig = buildPointingConfigChannels(s, Focus[State](_.pointingConfig)),
      absorbGuideDir =
        new TestChannel[F, State, CadDirective](s, Focus[State](_.absorbGuideDirState)),
      zeroGuideDir = new TestChannel[F, State, CadDirective](s, Focus[State](_.zeroGuideDirState)),
      instrumentOffset = buildInstrumentOffsetCommandChannels(s, Focus[State](_.instrumentOffset)),
      azimuthWrap = new TestChannel[F, State, String](s, Focus[State](_.azimuthWrap)),
      rotatorWrap = new TestChannel[F, State, String](s, Focus[State](_.rotatorWrap)),
      zeroRotatorGuideDir =
        new TestChannel[F, State, CadDirective](s, Focus[State](_.zeroRotatorGuide)),
      pwfs1Mechs =
        PwfsMechCmdChannels[F](new TestChannel[F, State, String](s, Focus[State](_.p1Filter)),
                               new TestChannel[F, State, String](s, Focus[State](_.p1FieldStop))
        ),
      pwfs2Mechs =
        PwfsMechCmdChannels[F](new TestChannel[F, State, String](s, Focus[State](_.p2Filter)),
                               new TestChannel[F, State, String](s, Focus[State](_.p2FieldStop))
        ),
      chopConfig = ChopConfigChannels[F](
        new TestChannel[F, State, String](s, Focus[State](_.chopConfig.typ)),
        new TestChannel[F, State, String](s, Focus[State](_.chopConfig.source)),
        new TestChannel[F, State, String](s, Focus[State](_.chopConfig.frequency)),
        new TestChannel[F, State, String](s, Focus[State](_.chopConfig.dutyCycle))
      ),
      chopRelative = ChopRelativeChannels[F](
        new TestChannel[F, State, String](s, Focus[State](_.chopRelative.thrw)),
        new TestChannel[F, State, String](s, Focus[State](_.chopRelative.angle)),
        new TestChannel[F, State, String](s, Focus[State](_.chopRelative.system)),
        new TestChannel[F, State, String](s, Focus[State](_.chopRelative.equinox))
      )
    )

  def build[F[_]: {Async, Parallel, Dispatcher}](s: Ref[F, State]): TcsEpicsSystem[F] = {
    val channels = buildChannels(s)
    TcsEpicsSystem.buildSystem(
      new TestApplyCommand[F],
      new TestObserveCommand[F](channels.guide.pwfs1Integrating),
      new TestObserveCommand[F](channels.guide.pwfs2Integrating),
      new TestObserveCommand[F](channels.guide.oiwfsIntegrating),
      channels
    )
  }

}
