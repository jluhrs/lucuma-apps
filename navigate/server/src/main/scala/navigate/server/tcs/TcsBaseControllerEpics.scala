// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import coulomb.*
import coulomb.units.accepted.ArcSecond
import fs2.Stream
import lucuma.core.enums
import lucuma.core.enums.ComaOption
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.M1Source
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Year
import lucuma.core.model.GuideConfig
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.ProbeGuide
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Focus.focus
import monocle.Getter
import monocle.Lens
import mouse.all.*
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.AutoparkAowfs
import navigate.model.AutoparkGems
import navigate.model.AutoparkOiwfs
import navigate.model.AutoparkPwfs1
import navigate.model.AutoparkPwfs2
import navigate.model.BafflesState
import navigate.model.Distance
import navigate.model.FocalPlaneOffset
import navigate.model.GuiderConfig
import navigate.model.HandsetAdjustment
import navigate.model.HandsetAdjustment.HorizontalAdjustment
import navigate.model.InstrumentSpecifics
import navigate.model.Origin
import navigate.model.PointingCorrections
import navigate.model.PwfsMechsState
import navigate.model.ResetPointing
import navigate.model.RotatorAngle
import navigate.model.RotatorTrackConfig
import navigate.model.RotatorTrackingMode
import navigate.model.ShortcircuitMountFilter
import navigate.model.ShortcircuitTargetFilter
import navigate.model.SlewOptions
import navigate.model.StopGuide
import navigate.model.SwapConfig
import navigate.model.Target
import navigate.model.Target.*
import navigate.model.TcsConfig
import navigate.model.TrackingConfig
import navigate.model.WfsConfiguration
import navigate.model.ZeroChopThrow
import navigate.model.ZeroGuideOffset
import navigate.model.ZeroInstrumentOffset
import navigate.model.ZeroMountDiffTrack
import navigate.model.ZeroMountOffset
import navigate.model.ZeroSourceDiffTrack
import navigate.model.ZeroSourceOffset
import navigate.model.enums
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.AoFoldPosition
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.DomeMode
import navigate.model.enums.HrwfsPickupPosition
import navigate.model.enums.LightSource
import navigate.model.enums.OiwfsWavelength
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server
import navigate.server.ApplyCommandResult
import navigate.server.ConnectionTimeout
import navigate.server.acm.CarState
import navigate.server.acm.ObserveCommand
import navigate.server.ephemeris.EphemerisFile
import navigate.server.epicsdata
import navigate.server.epicsdata.AgMechPosition
import navigate.server.epicsdata.BinaryOnOff
import navigate.server.epicsdata.BinaryYesNo
import navigate.server.tcs.AcquisitionCameraEpicsSystem.*
import navigate.server.tcs.AgsEpicsSystem.PwfsAngles
import navigate.server.tcs.ParkStatus.NotParked
import navigate.server.tcs.TcsEpicsSystem.ProbeGuideState
import navigate.server.tcs.TcsEpicsSystem.ProbeTrackingCommand
import navigate.server.tcs.TcsEpicsSystem.PwfsMechCommands
import navigate.server.tcs.TcsEpicsSystem.TargetCommand
import navigate.server.tcs.TcsEpicsSystem.TcsCommands
import navigate.server.tcs.TcsEpicsSystem.WavelengthCommand
import navigate.server.tcs.TcsEpicsSystem.WfsCommands
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

import TcsBaseController.{
  AcCommands,
  EquinoxDefault,
  FixedSystem,
  PwfsMechanismCommands,
  SystemDefault
}

/* This class implements the common TCS commands */
abstract class TcsBaseControllerEpics[F[_]: {Async, Parallel, Logger}](
  sys:      EpicsSystems[F],
  timeout:  FiniteDuration,
  stateRef: Ref[F, TcsBaseControllerEpics.State]
) extends TcsBaseController[F] {

  import TcsBaseControllerEpics.*

  private val McsParkTimeout = FiniteDuration(60, SECONDS)

  override def mcsPark: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(McsParkTimeout)
      .mcsParkCommand
      .mark
      .post
      .verifiedRun(ConnectionTimeout)

  override def mcsFollow(enable: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .mcsFollowCommand
      .setFollow(enable)
      .post
      .verifiedRun(ConnectionTimeout)

  override def rotStop(useBrakes: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .rotStopCommand
      .setBrakes(useBrakes)
      .post
      .verifiedRun(ConnectionTimeout)

  private val RotParkTimeout = FiniteDuration(60, SECONDS)

  override def rotPark: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(RotParkTimeout)
      .rotParkCommand
      .mark
      .post
      .verifiedRun(ConnectionTimeout)

  override def rotFollow(enable: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .rotFollowCommand
      .setFollow(enable)
      .post
      .verifiedRun(ConnectionTimeout)

  override def rotMove(angle: RotatorAngle): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .rotMoveCommand
      .setAngle(angle)
      .post
      .verifiedRun(ConnectionTimeout)

  override def ecsCarouselMode(
    domeMode:      DomeMode,
    shutterMode:   ShutterMode,
    slitHeight:    Double,
    domeEnable:    Boolean,
    shutterEnable: Boolean
  ): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .ecsCarouselModeCmd
      .setDomeMode(domeMode)
      .ecsCarouselModeCmd
      .setShutterMode(shutterMode)
      .ecsCarouselModeCmd
      .setSlitHeight(slitHeight)
      .ecsCarouselModeCmd
      .setDomeEnable(domeEnable)
      .ecsCarouselModeCmd
      .setShutterEnable(shutterEnable)
      .post
      .verifiedRun(ConnectionTimeout)

  private val EcsVentGatesMoveTimeout = FiniteDuration(60, SECONDS)

  override def ecsVentGatesMove(gateEast: Double, gateWest: Double): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(EcsVentGatesMoveTimeout)
      .ecsVenGatesMoveCmd
      .setVentGateEast(gateEast)
      .ecsVenGatesMoveCmd
      .setVentGateWest(gateWest)
      .post
      .verifiedRun(ConnectionTimeout)

  val DefaultBrightness: Double = 10.0

  protected def setTarget(
    l:      Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]],
    target: Target
  ): TcsCommands[F] => TcsCommands[F] = target match {
    case t: AzElTarget      =>
      { (x: TcsCommands[F]) => l.get(x).objectName(t.objectName) }
        .compose[TcsCommands[F]](l.get(_).coordSystem("AzEl"))
        .compose[TcsCommands[F]](
          l.get(_).coord1(Angle.fromStringSignedDMS.reverseGet(t.coordinates.azimuth.toAngle))
        )
        .compose[TcsCommands[F]](
          l.get(_).coord2(Angle.fromStringSignedDMS.reverseGet(t.coordinates.elevation.toAngle))
        )
        .compose[TcsCommands[F]](l.get(_).brightness(DefaultBrightness))
        .compose[TcsCommands[F]](l.get(_).epoch(2000.0))
        .compose[TcsCommands[F]](l.get(_).equinox(""))
        .compose[TcsCommands[F]](l.get(_).parallax(0.0))
        .compose[TcsCommands[F]](l.get(_).radialVelocity(0.0))
        .compose[TcsCommands[F]](l.get(_).properMotion1(0.0))
        .compose[TcsCommands[F]](l.get(_).properMotion2(0.0))
        .compose[TcsCommands[F]](l.get(_).ephemerisFile(""))
    case t: SiderealTarget  =>
      val properMotionRaScale =
        if (Math.cos(t.coordinates.dec.toRadians) =!= 0.0)
          1.0 / 15.0 / Math.cos(t.coordinates.dec.toRadians)
        else 0.0
      { (x: TcsCommands[F]) => l.get(x).objectName(t.objectName) }
        .compose[TcsCommands[F]](l.get(_).coordSystem(SystemDefault))
        .compose[TcsCommands[F]](
          l.get(_).coord1(HourAngle.fromStringHMS.reverseGet(t.coordinates.ra.toHourAngle))
        )
        .compose[TcsCommands[F]](
          l.get(_).coord2(Angle.fromStringSignedDMS.reverseGet(t.coordinates.dec.toAngle))
        )
        .compose[TcsCommands[F]](l.get(_).brightness(DefaultBrightness))
        .compose[TcsCommands[F]](l.get(_).epoch(t.epoch.epochYear))
        .compose[TcsCommands[F]](l.get(_).equinox(EquinoxDefault))
        .compose[TcsCommands[F]](
          l.get(_)
            .parallax(t.parallax.getOrElse(Parallax.Zero).mas.toUnit[ArcSecond].value.toDouble)
        )
        .compose[TcsCommands[F]](
          l.get(_)
            .radialVelocity(
              t.radialVelocity.getOrElse(RadialVelocity.Zero).toDoubleKilometersPerSecond
            )
        )
        .compose[TcsCommands[F]](
          l.get(_)
            .properMotion1(
              t.properMotion
                .getOrElse(ProperMotion.Zero)
                .ra
                .masy
                .toUnit[ArcSecond / Year]
                .value
                .toDouble * properMotionRaScale
            )
        )
        .compose[TcsCommands[F]](
          l.get(_)
            .properMotion2(
              t.properMotion
                .getOrElse(ProperMotion.Zero)
                .dec
                .masy
                .toUnit[ArcSecond / Year]
                .value
                .toDouble
            )
        )
        .compose[TcsCommands[F]](l.get(_).ephemerisFile(""))
    case t: EphemerisTarget =>
      { (x: TcsCommands[F]) => l.get(x).objectName(t.objectName) }
        .compose[TcsCommands[F]](l.get(_).coordSystem(""))
        .compose[TcsCommands[F]](l.get(_).coord1("00:00:00.000000"))
        .compose[TcsCommands[F]](l.get(_).coord2("00:00:00.000000"))
        .compose[TcsCommands[F]](l.get(_).brightness(DefaultBrightness))
        .compose[TcsCommands[F]](l.get(_).epoch(2000.0))
        .compose[TcsCommands[F]](l.get(_).equinox(""))
        .compose[TcsCommands[F]](l.get(_).parallax(0.0))
        .compose[TcsCommands[F]](l.get(_).radialVelocity(0.0))
        .compose[TcsCommands[F]](l.get(_).properMotion1(0.0))
        .compose[TcsCommands[F]](l.get(_).properMotion2(0.0))
        .compose[TcsCommands[F]](
          l.get(_).ephemerisFile(EphemerisFile.filenameFromKey(t.ephemerisKey))
        )
  }

  protected def setSourceAWalength(w: Wavelength): TcsCommands[F] => TcsCommands[F] =
    _.sourceAWavel.wavelength(w)

  protected def setSlewOptions(so: SlewOptions): TcsCommands[F] => TcsCommands[F] =
    (x: TcsCommands[F]) =>
      x.slewOptionsCommand
        .zeroChopThrow(ZeroChopThrow.value(so.zeroChopThrow))
        .slewOptionsCommand
        .zeroSourceOffset(ZeroSourceOffset.value(so.zeroSourceOffset))
        .slewOptionsCommand
        .zeroSourceDiffTrack(ZeroSourceDiffTrack.value(so.zeroSourceDiffTrack))
        .slewOptionsCommand
        .zeroMountOffset(ZeroMountOffset.value(so.zeroMountOffset))
        .slewOptionsCommand
        .zeroMountDiffTrack(ZeroMountDiffTrack.value(so.zeroMountDiffTrack))
        .slewOptionsCommand
        .shortcircuitTargetFilter(ShortcircuitTargetFilter.value(so.shortcircuitTargetFilter))
        .slewOptionsCommand
        .shortcircuitMountFilter(ShortcircuitMountFilter.value(so.shortcircuitMountFilter))
        .slewOptionsCommand
        .resetPointing(ResetPointing.value(so.resetPointing))
        .slewOptionsCommand
        .stopGuide(StopGuide.value(so.stopGuide))
        .slewOptionsCommand
        .zeroGuideOffset(ZeroGuideOffset.value(so.zeroGuideOffset))
        .slewOptionsCommand
        .zeroInstrumentOffset(ZeroInstrumentOffset.value(so.zeroInstrumentOffset))
        .slewOptionsCommand
        .autoparkPwfs1(AutoparkPwfs1.value(so.autoparkPwfs1))
        .slewOptionsCommand
        .autoparkPwfs2(AutoparkPwfs2.value(so.autoparkPwfs2))
        .slewOptionsCommand
        .autoparkOiwfs(AutoparkOiwfs.value(so.autoparkOiwfs))
        .slewOptionsCommand
        .autoparkGems(AutoparkGems.value(so.autoparkGems))
        .slewOptionsCommand
        .autoparkAowfs(AutoparkAowfs.value(so.autoparkAowfs))

  protected def setRotatorIaa(angle: Angle): TcsCommands[F] => TcsCommands[F] =
    (x: TcsCommands[F]) => x.rotatorCommand.iaa(angle)

  protected def setFocusOffset(offset: Distance): TcsCommands[F] => TcsCommands[F] =
    (x: TcsCommands[F]) => x.focusOffsetCommand.focusOffset(offset)

  protected def setOrigin(origin: Origin): TcsCommands[F] => TcsCommands[F] =
    (x: TcsCommands[F]) => x.originCommand.originX(origin.x).originCommand.originY(origin.y)

  protected def resetAllTracking: VerifiedEpics[F, F, ApplyCommandResult] =
    setProbeTracking(
      Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
        _.pwfs1ProbeTrackingCommand
      ),
      TrackingConfig.noTracking
    ).compose(
      setProbeTracking(
        Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
          _.pwfs2ProbeTrackingCommand
        ),
        TrackingConfig.noTracking
      )
    ).compose(
      setProbeTracking(
        Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
          _.oiwfsProbeTrackingCommand
        ),
        TrackingConfig.noTracking
      )
    )(sys.tcsEpics.startCommand(timeout))
      .post

  protected def stopAllWfs: F[ApplyCommandResult] =
    pwfs1StopObserve *>
      pwfs2StopObserve *>
      oiwfsStopObserve

  protected def applyTcsConfig(
    config:      TcsConfig,
    pwfs1Filter: PwfsFilter,
    pwfs2Filter: PwfsFilter
  ): TcsCommands[F] => TcsCommands[F] =
    setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.sourceACmd),
              config.sourceATarget
    ).compose(config.sourceATarget.wavelength.map(setSourceAWalength).getOrElse(identity))
      .compose(setRotatorTrackingConfig(config.rotatorTrackConfig))
      .compose(setInstrumentSpecifics(config.instrumentSpecifics))
      .compose(
        config.pwfs1
          .map(o =>
            setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.pwfs1TargetCmd),
                      o.target
            )
              .compose[TcsCommands[F]](_.pwfs1Wavel.wavelength(pwfs1Filter.wavel))
              .compose(
                setProbeTracking(Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
                                   _.pwfs1ProbeTrackingCommand
                                 ),
                                 o.tracking
                )
              )
          )
          .getOrElse(
            setProbeTracking(
              Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
                _.pwfs1ProbeTrackingCommand
              ),
              TrackingConfig.noTracking
            )
          )
      )
      .compose(
        config.pwfs2
          .map(o =>
            setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.pwfs2TargetCmd),
                      o.target
            )
              .compose[TcsCommands[F]](_.pwfs2Wavel.wavelength(pwfs2Filter.wavel))
              .compose(
                setProbeTracking(Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
                                   _.pwfs2ProbeTrackingCommand
                                 ),
                                 o.tracking
                )
              )
          )
          .getOrElse(
            setProbeTracking(
              Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
                _.pwfs2ProbeTrackingCommand
              ),
              TrackingConfig.noTracking
            )
          )
      )
      .compose(
        config.oiwfs
          .map(o =>
            setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.oiwfsTargetCmd),
                      o.target
            )
              .compose[TcsCommands[F]](x =>
                x.oiwfsWavel.wavelength(getOiwfsWavelegth(config.instrument))
              )
              .compose(
                setProbeTracking(Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
                                   _.oiwfsProbeTrackingCommand
                                 ),
                                 o.tracking
                )
              )
          )
          .getOrElse(
            setProbeTracking(
              Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
                _.oiwfsProbeTrackingCommand
              ),
              TrackingConfig.noTracking
            )
          )
      )
      .compose(
        (config.baffles, config.sourceATarget.wavelength)
          .mapN { case (b, w) =>
            setBaffles(b.central(w, config.instrument), b.deployable(w, config.instrument))
          }
          .getOrElse(identity)
      )

  protected def getOiwfsWavelegth(instrument: Instrument): Wavelength = (instrument match {
    case Instrument.Flamingos2 => OiwfsWavelength.Flamingos2Oiwfs
    case Instrument.GmosNorth  => OiwfsWavelength.GmosOiwfs
    case Instrument.GmosSouth  => OiwfsWavelength.GmosOiwfs
    case _                     => OiwfsWavelength.K
  }).wavel

  // The difference between this and the full applyTcsConfig is that here we keep the instrument parameters, but set
  // the origin for the AC.
  protected def applyPointToGuideConfig(
    config: TcsConfig
  ): TcsCommands[F] => TcsCommands[F] =
    setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.sourceACmd),
              config.sourceATarget
    ).compose(config.sourceATarget.wavelength.map(setSourceAWalength).getOrElse(identity))
      .compose(setRotatorTrackingConfig(config.rotatorTrackConfig))
      .compose(setOrigin(config.instrumentSpecifics.origin))

  private val TcsConfigTimeout = FiniteDuration(60, SECONDS)

  // Added a 1.5 s wait between selecting the OIWFS and setting targets, to copy TCC
  override def tcsConfig(config: TcsConfig): F[ApplyCommandResult] = for {
    _   <- sys.tcsEpics.clearErrors.verifiedRun(ConnectionTimeout)
    _   <- disableGuide
    p1f <- sys.ags.status.pwfs1Mechs.colFilter
             .verifiedRun(ConnectionTimeout)
             .attempt
             .map(_.toOption.flatten.getOrElse(PwfsFilter.Neutral))
    p2f <- sys.ags.status.pwfs2Mechs.colFilter
             .verifiedRun(ConnectionTimeout)
             .attempt
             .map(_.toOption.flatten.getOrElse(PwfsFilter.Neutral))
    r   <- (
             selectOiwfs(config) *>
               VerifiedEpics.liftF(Temporal[F].sleep(OiwfsSelectionDelay)) *>
               applyTcsConfig(config, p1f, p2f)(
                 sys.tcsEpics.startCommand(TcsConfigTimeout)
               ).post
           ).verifiedRun(ConnectionTimeout)
  } yield r

  override def slew(
    slewOptions: SlewOptions,
    tcsConfig:   TcsConfig
  ): F[ApplyCommandResult] = for {
    _   <- sys.tcsEpics.clearErrors.verifiedRun(ConnectionTimeout)
    _   <- (stopAllWfs *> disableGuide).whenA(slewOptions.stopGuide.value)
    _   <- disableTargetFilter.whenA(slewOptions.shortcircuitTargetFilter.value)
    p1f <- sys.ags.status.pwfs1Mechs.colFilter
             .verifiedRun(ConnectionTimeout)
             .attempt
             .map(_.toOption.flatten.getOrElse(PwfsFilter.Neutral))
    p2f <- sys.ags.status.pwfs2Mechs.colFilter
             .verifiedRun(ConnectionTimeout)
             .attempt
             .map(_.toOption.flatten.getOrElse(PwfsFilter.Neutral))
    r   <- (
             resetAllTracking *>
               selectOiwfsT(tcsConfig)
                 .andThen(
                   _.wrapsCommand.azimuth(0).wrapsCommand.rotator(0).zeroRotatorGuide.mark
                 )(sys.tcsEpics.startCommand(timeout))
                 .post *>
               VerifiedEpics.liftF(Temporal[F].sleep(OiwfsSelectionDelay)) *>
               applyTcsConfig(tcsConfig, p1f, p2f)
                 .andThen(c => c.targetFilter.shortcircuit(ShortcircuitTargetFilter(true)))
                 .andThen { c =>
                   c.instrumentOffsetCommand
                     .offsetX(Distance.Zero)
                     .instrumentOffsetCommand
                     .offsetY(Distance.Zero)
                 }
                 .andThen(resetChopParameters)
                 .andThen(setSlewOptions(slewOptions))(
                   sys.tcsEpics.startCommand(TcsConfigTimeout)
                 )
                 .post
           ).verifiedRun(ConnectionTimeout)
    // TODO: Consider case AO -> Instrument
    _   <- lightPath(LightSource.Sky, tcsConfig.instrument.toLightSink)
  } yield r

  private val defaultChopType: String       = "2posn"
  private val defaultChopSource: ChopSource = ChopSource.Ics0
  private val defaultChopFrequency: Double  = 3.05
  private val defaultChopDutyCycle: Double  = 87.0
  private val defaultChopThrow: Angle       = Angle.Angle0
  private val defaultChopAngle: Angle       = Angle.Angle0

  private def resetChopParameters(cmd: TcsCommands[F]): TcsCommands[F] =
    cmd.chopConfig
      .typ(defaultChopType)
      .chopConfig
      .source(defaultChopSource)
      .chopConfig
      .frequency(defaultChopFrequency)
      .chopConfig
      .dutyCycle(defaultChopDutyCycle)
      .chopRelative
      .thrw(defaultChopThrow)
      .chopRelative
      .angle(defaultChopAngle)
      .chopRelative
      .system(SystemDefault)
      .chopRelative
      .equinox(EquinoxDefault)

  protected def setInstrumentSpecifics(
    config: InstrumentSpecifics
  ): TcsCommands[F] => TcsCommands[F] =
    setRotatorIaa(config.iaa)
      .compose(setFocusOffset(config.focusOffset))
      .compose(setOrigin(config.origin))

  private val RotMoveTimeout = FiniteDuration(60, SECONDS)

  override def instrumentSpecifics(config: InstrumentSpecifics): F[ApplyCommandResult] =
    setInstrumentSpecifics(config)(
      sys.tcsEpics.startCommand(RotMoveTimeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  override def pwfs1Target(target: Target): F[ApplyCommandResult] =
    setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.pwfs1TargetCmd), target)(
      sys.tcsEpics.startCommand(timeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  override def pwfs2Target(target: Target): F[ApplyCommandResult] =
    setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.pwfs2TargetCmd), target)(
      sys.tcsEpics.startCommand(timeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  override def oiwfsTarget(target: Target): F[ApplyCommandResult] =
    setTarget(Getter[TcsCommands[F], TargetCommand[F, TcsCommands[F]]](_.oiwfsTargetCmd), target)(
      sys.tcsEpics.startCommand(timeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  override def rotIaa(angle: Angle): F[ApplyCommandResult] =
    setRotatorIaa(angle)(
      sys.tcsEpics.startCommand(RotMoveTimeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  protected def setProbeTracking(
    l:      Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]],
    config: TrackingConfig
  ): TcsCommands[F] => TcsCommands[F] = { (x: TcsCommands[F]) =>
    l.get(x).nodAchopA(config.nodAchopA)
  }
    .compose[TcsCommands[F]](l.get(_).nodAchopB(config.nodAchopB))
    .compose[TcsCommands[F]](l.get(_).nodBchopA(config.nodBchopA))
    .compose[TcsCommands[F]](l.get(_).nodBchopB(config.nodBchopB))

  override def pwfs1ProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    setProbeTracking(
      Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](_.pwfs1ProbeTrackingCommand),
      config
    )(
      sys.tcsEpics.startCommand(timeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  private val Pwfs1ParkTimeout = FiniteDuration(60, SECONDS)

  override def pwfs1Park: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(Pwfs1ParkTimeout)
      .pwfs1ProbeCommands
      .park
      .mark
      .post
      .verifiedRun(ConnectionTimeout)

  override def pwfs1Follow(enable: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .pwfs1ProbeCommands
      .follow
      .setFollow(enable)
      .post
      .verifiedRun(ConnectionTimeout)

  override def pwfs2ProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    setProbeTracking(
      Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](_.pwfs2ProbeTrackingCommand),
      config
    )(
      sys.tcsEpics.startCommand(timeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  private val Pwfs2ParkTimeout = FiniteDuration(60, SECONDS)

  override def pwfs2Park: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(Pwfs2ParkTimeout)
      .pwfs2ProbeCommands
      .park
      .mark
      .post
      .verifiedRun(ConnectionTimeout)

  override def pwfs2Follow(enable: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .pwfs2ProbeCommands
      .follow
      .setFollow(enable)
      .post
      .verifiedRun(ConnectionTimeout)

  override def oiwfsProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    setProbeTracking(
      Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](_.oiwfsProbeTrackingCommand),
      config
    )(
      sys.tcsEpics.startCommand(timeout)
    ).post
      .verifiedRun(ConnectionTimeout)

  private val OiwfsParkTimeout = FiniteDuration(60, SECONDS)

  override def oiwfsPark: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(OiwfsParkTimeout)
      .oiwfsProbeCommands
      .park
      .mark
      .post
      .verifiedRun(ConnectionTimeout)

  override def oiwfsFollow(enable: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .oiwfsProbeCommands
      .follow
      .setFollow(enable)
      .post
      .verifiedRun(ConnectionTimeout)

  def setRotatorTrackingConfig(cfg: RotatorTrackConfig): TcsCommands[F] => TcsCommands[F] =
    (x: TcsCommands[F]) =>
      cfg.mode match {
        case RotatorTrackingMode.Fixed    =>
          x.rotMoveCommand
            .setAngle(RotatorAngle.fromAngle(cfg.ipa))
            .rotatorCommand
            .ipa(cfg.ipa)
            .rotatorCommand
            .system(FixedSystem)
        case RotatorTrackingMode.Tracking =>
          x.rotatorCommand
            .ipa(cfg.ipa)
            .rotatorCommand
            .system(SystemDefault)
            .rotatorCommand
            .equinox(EquinoxDefault)
      }

  override def rotTrackingConfig(cfg: RotatorTrackConfig): F[ApplyCommandResult] =
    setRotatorTrackingConfig(cfg)(sys.tcsEpics.startCommand(timeout)).post
      .verifiedRun(ConnectionTimeout)

  private def calcM2Guide(
    state: ProbeGuideState[F]
  ): VerifiedEpics[F, F, M2BeamConfig] = {
    def calcBeams(chopA: BinaryOnOff, chopB: BinaryOnOff): M2BeamConfig = (chopA, chopB) match {
      case (BinaryOnOff.Off, BinaryOnOff.Off) => M2BeamConfig.None
      case (BinaryOnOff.Off, BinaryOnOff.On)  => M2BeamConfig.BeamB
      case (BinaryOnOff.On, BinaryOnOff.On)   => M2BeamConfig.BeamAB
      case (BinaryOnOff.On, BinaryOnOff.Off)  => M2BeamConfig.BeamA
    }

    for {
      faa  <- state.nodAchopA
      fab  <- state.nodAchopB
      fba  <- state.nodBchopA
      fbb  <- state.nodBchopB
      fnod <- sys.tcsEpics.status.nodState
    } yield for {
      aa  <- faa
      ab  <- fab
      ba  <- fba
      bb  <- fbb
      nod <- fnod
    } yield nod match
      case epicsdata.NodState.A => calcBeams(aa, ab)
      case epicsdata.NodState.B => calcBeams(ba, bb)
      case epicsdata.NodState.C => M2BeamConfig.None
  }

  private def enableM2Guide(
    cfg: M2GuideConfig.M2GuideOn
  ): VerifiedEpics[F, F, ApplyCommandResult] = {
    def setM2Guide(
      src:   TipTiltSource,
      beams: M2BeamConfig
    ): VerifiedEpics[F, F, ApplyCommandResult] =
      (beams =!= M2BeamConfig.None).fold(
        sys.tcsEpics
          .startCommand(timeout)
          .m2GuideConfigCommand
          .source(src.tag.toUpperCase)
          .m2GuideConfigCommand
          .sampleFreq(200.0)
          .m2GuideConfigCommand
          .filter("raw")
          .m2GuideConfigCommand
          .freq1(none)
          .m2GuideConfigCommand
          .freq2(none)
          .m2GuideConfigCommand
          .beam(beams.tag.toUpperCase)
          .m2GuideConfigCommand
          .reset(false)
          .post,
        VerifiedEpics.pureF(ApplyCommandResult.Completed)
      )

    sys.tcsEpics
      .startCommand(timeout)
      .m2GuideModeCommand
      .coma(cfg.coma === ComaOption.ComaOn)
      .post *> (
      for {
        oif  <- cfg.sources
                  .contains(TipTiltSource.OIWFS)
                  .fold(calcM2Guide(sys.tcsEpics.status.oiwfsProbeGuideState),
                        VerifiedEpics.pureF(M2BeamConfig.None)
                  )
        p1f  <- cfg.sources
                  .contains(TipTiltSource.PWFS1)
                  .fold(calcM2Guide(sys.tcsEpics.status.pwfs1ProbeGuideState),
                        VerifiedEpics.pureF(M2BeamConfig.None)
                  )
        p2f  <- cfg.sources
                  .contains(TipTiltSource.PWFS2)
                  .fold(calcM2Guide(sys.tcsEpics.status.pwfs2ProbeGuideState),
                        VerifiedEpics.pureF(M2BeamConfig.None)
                  )
        oicf <- sys.tcsEpics.status.m2oiGuide.map(_.map(M2BeamConfig.fromTcsGuideConfig))
        p1cf <- sys.tcsEpics.status.m2p1Guide.map(_.map(M2BeamConfig.fromTcsGuideConfig))
        p2cf <- sys.tcsEpics.status.m2p2Guide.map(_.map(M2BeamConfig.fromTcsGuideConfig))
      } yield for {
        oi  <- oif
        p1  <- p1f
        p2  <- p2f
        oic <- oicf
        p1c <- p1cf
        p2c <- p2cf
        r   <- {
          val mustReset = oi =!= oic || p1 =!= p1c || p2 =!= p2c
          mustReset.fold(
            (sys.tcsEpics.startCommand(timeout).m2GuideCommand.state(false).post *>
              sys.tcsEpics.startCommand(timeout).m2GuideResetCommand.mark.post *>
              setM2Guide(TipTiltSource.PWFS1, p1) *>
              setM2Guide(TipTiltSource.PWFS2, p2) *>
              setM2Guide(TipTiltSource.OIWFS, oi)).verifiedRun(ConnectionTimeout),
            ApplyCommandResult.Completed.pure[F]
          ) <*
            Logger[F]
              .info(
                "Cannot configure M2 guiding with OIWFS without a nod/chop guide configuration"
              )
              .whenA(cfg.sources.contains(TipTiltSource.OIWFS) && oi === M2BeamConfig.None) <*
            Logger[F]
              .info(
                "Cannot configure M2 guiding with PWFS1 without a nod/chop guide configuration"
              )
              .whenA(cfg.sources.contains(TipTiltSource.PWFS1) && p1 === M2BeamConfig.None) <*
            Logger[F]
              .info(
                "Cannot configure M2 guiding with PWFS2 without a nod/chop guide configuration"
              )
              .whenA(cfg.sources.contains(TipTiltSource.PWFS2) && p2 === M2BeamConfig.None)
        }
      } yield r
    ) *>
      sys.tcsEpics.startCommand(timeout).m2GuideCommand.state(true).post

  }

  override def enableGuide(config: TelescopeGuideConfig): F[ApplyCommandResult] = {
    val gains =
      if (config.dayTimeMode.exists(_ === true))
        dayTimeGains
      else
        defaultGains

    val m1 = config.m1Guide match {
      case M1GuideConfig.M1GuideOff        =>
        sys.tcsEpics.startCommand(CommandAcknowledgeTimeout).m1GuideCommand.state(false).post
      case M1GuideConfig.M1GuideOn(source) =>
        sys.tcsEpics
          .startCommand(CommandAcknowledgeTimeout)
          .m1GuideCommand
          .state(true)
          .m1GuideConfigCommand
          .source(source.tag.toUpperCase)
          .m1GuideConfigCommand
          .weighting("none")
          .m1GuideConfigCommand
          .frames(1)
          .m1GuideConfigCommand
          .filename("")
          .post
    }

    val m2 = config.m2Guide match {
      case M2GuideConfig.M2GuideOff          =>
        sys.tcsEpics
          .startCommand(CommandAcknowledgeTimeout)
          .m2GuideCommand
          .state(false)
          .m2GuideModeCommand
          .coma(false)
          .post
      case x @ M2GuideConfig.M2GuideOn(_, _) => enableM2Guide(x)
    }

    (gains *>
      m1 *>
      m2 *>
      sys.tcsEpics
        .startCommand(CommandAcknowledgeTimeout)
        .probeGuideModeCommand
        .setMode(config.probeGuide)
        .mountGuideCommand
        .mode(config.mountGuide === MountGuideOption.MountGuideOn)
        .mountGuideCommand
        .source("SCS")
        .post).verifiedRun(ConnectionTimeout) <*
      stateRef.get.flatMap { s =>
        s.pwfs1.period
          .flatMap(p =>
            guideUsesPwfs1(config.m1Guide, config.m2Guide)
              .option(setupPwfs1Observe(p, false))
          )
          .getOrElse(
            ApplyCommandResult.Completed.pure[F]
          ) *>
          s.pwfs2.period
            .flatMap(p =>
              guideUsesPwfs2(config.m1Guide, config.m2Guide)
                .option(setupPwfs2Observe(p, false))
            )
            .getOrElse(
              ApplyCommandResult.Completed.pure[F]
            ) *>
          s.oiwfs.period
            .flatMap(p =>
              guideUsesOiwfs(config.m1Guide, config.m2Guide)
                .option(setupOiwfsObserve(p, false))
            )
            .getOrElse(
              ApplyCommandResult.Completed.pure[F]
            )
      }
  }

  override def disableGuide: F[ApplyCommandResult] = sys.tcsEpics
    .startCommand(CommandAcknowledgeTimeout)
    .m1GuideCommand
    .state(false)
    .m2GuideModeCommand
    .coma(false)
    .m2GuideCommand
    .state(false)
    .mountGuideCommand
    .mode(false)
    .probeGuideModeCommand
    .setMode(None)
    .post
    .verifiedRun(ConnectionTimeout) <*
    stateRef.get.flatMap { s =>
      s.pwfs1.period
        .map(setupPwfs1Observe(_, true))
        .getOrElse(
          ApplyCommandResult.Completed.pure[F]
        ) *> s.pwfs2.period
        .map(setupPwfs2Observe(_, true))
        .getOrElse(
          ApplyCommandResult.Completed.pure[F]
        ) *> s.oiwfs.period
        .map(setupOiwfsObserve(_, true))
        .getOrElse(
          ApplyCommandResult.Completed.pure[F]
        )
    }

  def pauseGuide: F[ApplyCommandResult] = sys.tcsEpics
    .startCommand(CommandAcknowledgeTimeout)
    .m1GuideCommand
    .state(false)
    .m2GuideModeCommand
    .coma(false)
    .m2GuideCommand
    .state(false)
    .mountGuideCommand
    .mode(false)
    .probeGuideModeCommand
    .setMode(None)
    .post
    .verifiedRun(ConnectionTimeout)

  def resumeGuide(config: TelescopeGuideConfig): F[ApplyCommandResult] = {
    val comaVal = config.m2Guide match {
      case M2GuideConfig.M2GuideOff         => false
      case M2GuideConfig.M2GuideOn(coma, _) => coma === ComaOption.ComaOn
    }

    sys.tcsEpics
      .startCommand(CommandAcknowledgeTimeout)
      .m1GuideCommand
      .state(config.m1Guide =!= M1GuideConfig.M1GuideOff)
      .m2GuideCommand
      .state(config.m2Guide =!= M2GuideConfig.M2GuideOff)
      .m2GuideModeCommand
      .coma(comaVal)
      .probeGuideModeCommand
      .setMode(config.probeGuide)
      .mountGuideCommand
      .mode(config.mountGuide === MountGuideOption.MountGuideOn)
      .post
      .verifiedRun(ConnectionTimeout)
  }

  def darkFileName(path: String, prefix: String, exposureTime: TimeSpan): String = {
    val prf = if (prefix.isEmpty || prefix.lastOption.exists(_ === '_')) prefix else s"${prefix}_"
    val pth = if (path.isEmpty || path.lastOption.exists(_ === '/')) path else s"${path}/"

    if (exposureTime > TimeSpan.unsafeFromMicroseconds(1000000))
      s"${pth}${prf}${math.round(1000.0 / exposureTime.toSeconds.toDouble)}mHz.fits"
    else
      s"${pth}${prf}${math.round(1.0 / exposureTime.toSeconds.toDouble)}Hz.fits"
  }

  val dataFolderName: String = "data"

  def setupWfsObserve(
    prefix:       String,
    state:        Lens[State, WfsConfigState],
    active:       VerifiedEpics[F, F, BinaryYesNo],
    darkFileProc: String => VerifiedEpics[F, F, ApplyCommandResult],
    z2m2Proc:     Int => VerifiedEpics[F, F, ApplyCommandResult],
    wfs:          WfsCommands[F]
  )(exposureTime: TimeSpan, isQL: Boolean): F[ApplyCommandResult] =
    stateRef.get.flatMap { st =>
      val expTimeChange = state.get(st).period.forall(_ =!= exposureTime).option(exposureTime)
      val qlChange      = state.get(st).configuredForQl.forall(_ =!= isQL).option(isQL)

      val setSigProc  = expTimeChange
        .map(t => darkFileProc(darkFileName(dataFolderName, prefix, t)))
        .getOrElse(VerifiedEpics.pureF[F, F, ApplyCommandResult](ApplyCommandResult.Completed)) *>
        qlChange
          .map(
            _.fold(
              z2m2Proc(0),
              z2m2Proc(1)
            )
          )
          .getOrElse(VerifiedEpics.pureF[F, F, ApplyCommandResult](ApplyCommandResult.Completed))
      val setInterval =
        (c: WfsCommands[F]) => expTimeChange.fold(c)(t => c.observe.interval(t.toSeconds.toDouble))
      val setQl       = (c: WfsCommands[F]) =>
        qlChange.fold(c)(i =>
          c.observe.output(i.fold("QL", "")).observe.options(i.fold("DHS", "NONE"))
        )

      val setupAndStart: VerifiedEpics[F, F, ApplyCommandResult] = setSigProc *>
        (setInterval >>> setQl)(wfs).observe
          .numberOfExposures(-1)
          .observe
          .path("")
          .observe
          .fileName("")
          .observe
          .label("")
          .post(ObserveCommand.CommandType.PermanentOn)

      (for {
        wfsActive <- active.map(_.map(_ === BinaryYesNo.Yes))
        ret       <- VerifiedEpics.ifF[F, F, ApplyCommandResult](
                       wfsActive.map(_ && qlChange.isEmpty && expTimeChange.isEmpty)
                     ) {
                       VerifiedEpics.pureF[F, F, ApplyCommandResult](ApplyCommandResult.Completed)
                     } {
                       VerifiedEpics.ifF(wfsActive) {
                         wfs.stop.mark
                           .post(ObserveCommand.CommandType.PermanentOff)
                       } {
                         VerifiedEpics.pureF[F, F, ApplyCommandResult](ApplyCommandResult.Completed)
                       } *>
                         setupAndStart
                     }
      } yield ret).verifiedRun(ConnectionTimeout) <*
        stateRef.update(
          state.andThen(Focus[WfsConfigState](_.period)).replace(exposureTime.some) >>>
            state
              .andThen(Focus[WfsConfigState](_.configuredForQl))
              .replace(isQL.some)
        )
    }

  private val WfsObserveTimeout = FiniteDuration(20, SECONDS)

  private def setupPwfs1Observe: (exposureTime: TimeSpan, isQL: Boolean) => F[ApplyCommandResult] =
    setupWfsObserve(
      "p1",
      Focus[State](_.pwfs1),
      sys.tcsEpics.status.pwfs1On,
      (f: String) =>
        sys.tcsEpics.startCommand(timeout).pwfs1Commands.signalProc.darkFilename(f).post,
      (z: Int) => sys.tcsEpics.startCommand(timeout).pwfs1Commands.closedLoop.zernikes2m2(z).post,
      sys.tcsEpics.startPwfs1Command(WfsObserveTimeout)
    )

  override def pwfs1Observe(exposureTime: TimeSpan): F[ApplyCommandResult] =
    getGuideState.flatMap { g =>
      setupPwfs1Observe(exposureTime, !guideUsesPwfs1(g.m1Guide, g.m2Guide))
    }

  private def setupPwfs2Observe: (exposureTime: TimeSpan, isQL: Boolean) => F[ApplyCommandResult] =
    setupWfsObserve(
      "p2",
      Focus[State](_.pwfs2),
      sys.tcsEpics.status.pwfs2On,
      (f: String) =>
        sys.tcsEpics.startCommand(timeout).pwfs2Commands.signalProc.darkFilename(f).post,
      (z: Int) => sys.tcsEpics.startCommand(timeout).pwfs2Commands.closedLoop.zernikes2m2(z).post,
      sys.tcsEpics.startPwfs2Command(WfsObserveTimeout)
    )

  override def pwfs2Observe(exposureTime: TimeSpan): F[ApplyCommandResult] =
    getGuideState.flatMap { g =>
      setupPwfs2Observe(exposureTime, !guideUsesPwfs2(g.m1Guide, g.m2Guide))
    }

  private def setupOiwfsObserve: (exposureTime: TimeSpan, isQL: Boolean) => F[ApplyCommandResult] =
    setupWfsObserve(
      "",
      Focus[State](_.oiwfs),
      sys.tcsEpics.status.oiwfsOn,
      (f: String) => sys.oiwfs.startSignalProcCommand(timeout).filename(f).post,
      (z: Int) => sys.oiwfs.startClosedLoopCommand(timeout).zernikes2m2(z).post,
      sys.tcsEpics.startOiwfsCommand(WfsObserveTimeout)
    )

  override def oiwfsObserve(exposureTime: TimeSpan): F[ApplyCommandResult] =
    getGuideState.flatMap { g =>
      setupOiwfsObserve(exposureTime, !guideUsesOiwfs(g.m1Guide, g.m2Guide))
    }

  private val WfsStopObserveTimeout = FiniteDuration(15, SECONDS)

  override def pwfs1StopObserve: F[ApplyCommandResult] = sys.tcsEpics
    .startPwfs1Command(WfsStopObserveTimeout)
    .stop
    .mark
    .post(ObserveCommand.CommandType.PermanentOff)
    .verifiedRun(ConnectionTimeout) <*
    stateRef.update(
      _.focus(_.pwfs1.period)
        .replace(None)
        .focus(_.pwfs1.configuredForQl)
        .replace(None)
    )

  override def pwfs2StopObserve: F[ApplyCommandResult] = sys.tcsEpics
    .startPwfs2Command(WfsStopObserveTimeout)
    .stop
    .mark
    .post(ObserveCommand.CommandType.PermanentOff)
    .verifiedRun(ConnectionTimeout) <*
    stateRef.update(
      _.focus(_.pwfs2.period)
        .replace(None)
        .focus(_.pwfs2.configuredForQl)
        .replace(None)
    )

  override def oiwfsStopObserve: F[ApplyCommandResult] = sys.tcsEpics
    .startOiwfsCommand(WfsStopObserveTimeout)
    .stop
    .mark
    .post(ObserveCommand.CommandType.PermanentOff)
    .verifiedRun(ConnectionTimeout) <*
    stateRef.update(
      _.focus(_.oiwfs.period)
        .replace(None)
        .focus(_.oiwfs.configuredForQl)
        .replace(None)
    )

  def getProbeGuideState(chopnod: ProbeGuideState[F]): VerifiedEpics[F, F, TrackingConfig] = for {
    aaf <- chopnod.nodAchopA
    abf <- chopnod.nodAchopB
    baf <- chopnod.nodBchopA
    bbf <- chopnod.nodBchopB
  } yield for {
    aa <- aaf
    ab <- abf
    ba <- baf
    bb <- bbf
  } yield TrackingConfig(aa === BinaryOnOff.On,
                         ab === BinaryOnOff.On,
                         ba === BinaryOnOff.On,
                         bb === BinaryOnOff.On
  )

  def getProbesGuideState: VerifiedEpics[F, F, WfsGuideStates] = for {
    p1f <- getProbeGuideState(sys.tcsEpics.status.pwfs1ProbeGuideState)
    p2f <- getProbeGuideState(sys.tcsEpics.status.pwfs2ProbeGuideState)
    oif <- getProbeGuideState(sys.tcsEpics.status.oiwfsProbeGuideState)
  } yield for {
    p1 <- p1f
    p2 <- p2f
    oi <- oif
  } yield WfsGuideStates(p1, p2, oi)

  def pauseWfsTracking(current: WfsGuideStates): VerifiedEpics[F, F, ApplyCommandResult] = {
    val params = List(
      current.pwfs1.active.option(
        setProbeTracking(
          Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
            _.pwfs1ProbeTrackingCommand
          ),
          TrackingConfig.noTracking
        )
      ),
      current.pwfs2.active.option(
        setProbeTracking(
          Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
            _.pwfs2ProbeTrackingCommand
          ),
          TrackingConfig.noTracking
        )
      ),
      current.oiwfs.active.option(
        setProbeTracking(
          Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
            _.oiwfsProbeTrackingCommand
          ),
          TrackingConfig.noTracking
        )
      )
    ).flattenOption

    if (params.isEmpty) VerifiedEpics.pureF(ApplyCommandResult.Completed)
    else
      params.reduce((a, b) => a.compose(b))(sys.tcsEpics.startCommand(timeout)).post
  }

  def resumeWfsTracking(current: WfsGuideStates): VerifiedEpics[F, F, ApplyCommandResult] = {
    val params = List(
      current.pwfs1.active.option(
        setProbeTracking(
          Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
            _.pwfs1ProbeTrackingCommand
          ),
          current.pwfs1
        )
      ),
      current.pwfs2.active.option(
        setProbeTracking(
          Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
            _.pwfs2ProbeTrackingCommand
          ),
          current.pwfs2
        )
      ),
      current.oiwfs.active.option(
        setProbeTracking(
          Getter[TcsCommands[F], ProbeTrackingCommand[F, TcsCommands[F]]](
            _.oiwfsProbeTrackingCommand
          ),
          current.oiwfs
        )
      )
    ).flattenOption

    if (params.isEmpty) VerifiedEpics.pureF(ApplyCommandResult.Completed)
    else
      params.reduce((a, b) => a.compose(b))(sys.tcsEpics.startCommand(timeout)).post
  }

  def skyOffset(size: Angle): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .targetAdjustCommand
      .frame(ReferenceFrame.Tracking)
      .targetAdjustCommand
      .angle(Angle.Angle0)
      .targetAdjustCommand
      .size(Angle.signedDecimalArcseconds.get(size).doubleValue)
      .targetAdjustCommand
      .vtMask(List(VirtualTelescope.SourceA))
      .post
      .verifiedRun(ConnectionTimeout)

  // Number of frames taken for a Sky image.
  val SkyFrames: Int   = 200
  // Offset to take the Sky
  val SkyOffset: Angle = Angle.fromDoubleArcseconds(60.0)

  def takeWfsSky(
    prefix:          String,
    active:          VerifiedEpics[F, F, BinaryYesNo],
    cmds:            FiniteDuration => WfsCommands[F],
    darkFilenameCmd: String => VerifiedEpics[F, F, ApplyCommandResult],
    observeCmd:      TimeSpan => F[ApplyCommandResult]
  )(exposureTime: TimeSpan): F[ApplyCommandResult] = {
    val expTimeout: FiniteDuration = FiniteDuration(exposureTime.toMicroseconds,
                                                    TimeUnit.MICROSECONDS
    ) * (SkyFrames * 1.1).longValue + WfsObserveTimeout
    val postStopDelay              = FiniteDuration(10, TimeUnit.MILLISECONDS)
    val postDarkConfigDelay        = FiniteDuration(4, TimeUnit.SECONDS)
    val postObserveDelay           = FiniteDuration(50, TimeUnit.MILLISECONDS)

    for {
      wfsActive <-
        active.map(_.map(_ === BinaryYesNo.Yes)).verifiedRun(ConnectionTimeout)
      _         <- (cmds(WfsStopObserveTimeout).stop.mark
                     .post(ObserveCommand.CommandType.PermanentOff)
                     .verifiedRun(ConnectionTimeout) *> Temporal[F].sleep(postStopDelay)).whenA(wfsActive)
      _         <- darkFilenameCmd(darkFileName("", prefix, exposureTime))
                     .verifiedRun(ConnectionTimeout) *> Temporal[F].sleep(postDarkConfigDelay)
      ret       <- cmds(expTimeout).observe
                     .numberOfExposures(SkyFrames)
                     .observe
                     .interval(exposureTime.toSeconds.toDouble)
                     .post(ObserveCommand.CommandType.TemporarlyOn)
                     .verifiedRun(ConnectionTimeout) <* Temporal[F].sleep(postObserveDelay)
      _         <- observeCmd(exposureTime).whenA(wfsActive)
    } yield ret
  }

  def takePwfs1Sky(guide: GuideConfig): TimeSpan => F[ApplyCommandResult] = takeWfsSky(
    "p1",
    sys.tcsEpics.status.pwfs1On,
    sys.tcsEpics.startPwfs1Command,
    (fn: String) => sys.tcsEpics.startCommand(timeout).pwfs1Commands.dark.filename(fn).post,
    t => setupPwfs1Observe(t, !guideUsesPwfs1(guide.tcsGuide.m1Guide, guide.tcsGuide.m2Guide))
  )

  def takePwfs2Sky(guide: GuideConfig): TimeSpan => F[ApplyCommandResult] = takeWfsSky(
    "p2",
    sys.tcsEpics.status.pwfs2On,
    sys.tcsEpics.startPwfs2Command,
    (fn: String) => sys.tcsEpics.startCommand(timeout).pwfs2Commands.dark.filename(fn).post,
    t => setupPwfs2Observe(t, !guideUsesPwfs2(guide.tcsGuide.m1Guide, guide.tcsGuide.m2Guide))
  )

  def takeOiwfsSky(guide: GuideConfig): TimeSpan => F[ApplyCommandResult] = takeWfsSky(
    "",
    sys.tcsEpics.status.oiwfsOn,
    sys.tcsEpics.startOiwfsCommand,
    (fn: String) => sys.oiwfs.startDarkCommand(timeout).filename(fn).post,
    t => setupOiwfsObserve(t, !guideUsesOiwfs(guide.tcsGuide.m1Guide, guide.tcsGuide.m2Guide))
  )

  def disableTargetFilter: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .targetFilter
      .shortcircuit(ShortcircuitTargetFilter(true))
      .post
      .verifiedRun(ConnectionTimeout)

  private def wfsSky(
    takeSky: GuideConfig => TimeSpan => F[ApplyCommandResult]
  )(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] = for {
    pg <- getProbesGuideState.verifiedRun(ConnectionTimeout)
    _  <- disableTargetFilter
    _  <- pauseGuide
    _  <- pauseWfsTracking(pg).verifiedRun(ConnectionTimeout)
    _  <- skyOffset(SkyOffset)
    r  <- takeSky(guide)(exposureTime)
    _  <- skyOffset(-SkyOffset)
    _  <- resumeWfsTracking(pg).verifiedRun(ConnectionTimeout)
    _  <- resumeGuide(guide.tcsGuide)
  } yield r

  override def pwfs1Sky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    wfsSky(takePwfs1Sky)(exposureTime)(guide)

  override def pwfs2Sky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    wfsSky(takePwfs2Sky)(exposureTime)(guide)

  override def oiwfsSky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    wfsSky(takeOiwfsSky)(exposureTime)(guide)

  // Time to wait after selecting the OIWFS in the AG Sequencer, to let the values propagate to TCS.
  private val OiwfsSelectionDelay: Duration = 1500.milliseconds

  private def selectOiwfsT(tcsConfig: TcsConfig)(c: TcsCommands[F]): TcsCommands[F] =
    c.oiwfsSelectCommand
      .oiwfsName(TcsBaseControllerEpics.encodeOiwfsSelect(tcsConfig.oiwfs, tcsConfig.instrument))
      .oiwfsSelectCommand
      .output("WFS")

  private def selectOiwfs(tcsConfig: TcsConfig): VerifiedEpics[F, F, ApplyCommandResult] =
    selectOiwfsT(tcsConfig)(sys.tcsEpics.startCommand(timeout)).post

  private def calcM1Guide(m1: BinaryOnOff, m1src: String): M1GuideConfig =
    if (m1 === BinaryOnOff.Off) M1GuideConfig.M1GuideOff
    else
      Enumerated[M1Source]
        .fromTag(m1src.toLowerCase.capitalize)
        .map(M1GuideConfig.M1GuideOn.apply)
        .getOrElse(M1GuideConfig.M1GuideOff)

  private def calcM2Source(v: String, tt: TipTiltSource): Set[TipTiltSource] =
    if (v.contains("AUTO")) Set(tt)
    else Set.empty

  private def calcM2Guide(
    m2:   BinaryOnOff,
    m2p1: String,
    m2p2: String,
    m2oi: String,
    m2ao: String,
    coma: BinaryOnOff
  ): M2GuideConfig = {
    val src = Set.empty ++
      calcM2Source(m2p1, TipTiltSource.PWFS1) ++
      calcM2Source(m2p2, TipTiltSource.PWFS2) ++
      calcM2Source(m2oi, TipTiltSource.OIWFS) ++
      calcM2Source(m2ao, TipTiltSource.GAOS)

    if (m2 === BinaryOnOff.On && src.nonEmpty)
      M2GuideConfig.M2GuideOn(ComaOption(coma === BinaryOnOff.On), src)
    else M2GuideConfig.M2GuideOff
  }

  def decodeGuideProbe(str: String, oiSel: Option[Instrument]): Option[GuideProbe] = str match {
    case "PWFS1" => GuideProbe.PWFS1.some
    case "PWFS2" => GuideProbe.PWFS2.some
    case "OIWFS" =>
      oiSel.flatMap {
        case Instrument.GmosSouth | Instrument.GmosNorth => GuideProbe.GmosOIWFS.some
        case Instrument.Flamingos2                       => GuideProbe.Flamingos2OIWFS.some
        case _                                           => None
      }
    case _       => none
  }

  private def calcProbeGuide(
    enabled: BinaryOnOff,
    from:    Option[GuideProbe],
    to:      Option[GuideProbe]
  ): Option[ProbeGuide] = for {
    src <- from
    snk <- to
    r   <- (enabled === BinaryOnOff.On).option(ProbeGuide(src, snk))
  } yield r

  override def getGuideState: F[GuideState] = {
    val x = for {
      fa <- sys.tcsEpics.status.m1Guide
      fb <- sys.tcsEpics.status.m2aoGuide
      fc <- sys.tcsEpics.status.m2oiGuide
      fd <- sys.tcsEpics.status.m2p1Guide
      fe <- sys.tcsEpics.status.m2p2Guide
      ff <- sys.tcsEpics.status.absorbTipTilt
      fg <- sys.tcsEpics.status.comaCorrect
      fh <- sys.tcsEpics.status.m1GuideSource
      fi <- sys.tcsEpics.status.m2GuideState
      fj <- sys.tcsEpics.status.pwfs1On
      fk <- sys.tcsEpics.status.pwfs2On
      fl <- sys.tcsEpics.status.oiwfsOn
      fm <- sys.hrwfs.status.observe
      fn <- sys.tcsEpics.status.probeGuideEnabled
      fo <- sys.tcsEpics.status.probeGuideSource
      fp <- sys.tcsEpics.status.probeGuideSink
      fq <- sys.ags.status.oiwfsName
    } yield for {
      a <- fa
      b <- fb
      c <- fc
      d <- fd
      e <- fe
      f <- ff
      g <- fg
      h <- fh
      i <- fi
      j <- fj
      k <- fk
      l <- fl
      m <- fm
      n <- fn
      o <- fo
      p <- fp
      q <- fq
    } yield GuideState(
      MountGuideOption(f =!= 0),
      calcM1Guide(a, h),
      calcM2Guide(i, d, e, c, b, g),
      calcProbeGuide(n, decodeGuideProbe(o, q), decodeGuideProbe(p, q)),
      j === BinaryYesNo.Yes,
      k === BinaryYesNo.Yes,
      l === BinaryYesNo.Yes,
      m === CarState.BUSY
    )

    x.verifiedRun(ConnectionTimeout)
  }

  def oiwfsDaytimeGains: VerifiedEpics[F, F, ApplyCommandResult] = sys.oiwfs
    .startGainCommand(timeout)
    .gains
    .setTipGain(0.0)
    .gains
    .setTiltGain(0.0)
    .gains
    .setFocusGain(0.0)
    .post

  def dayTimeGains: VerifiedEpics[F, F, ApplyCommandResult] =
    sys.pwfs1
      .startGainCommand(timeout)
      .gains
      .setTipGain(0.0)
      .gains
      .setTiltGain(0.0)
      .gains
      .setFocusGain(0.0)
      .post *>
      sys.pwfs2
        .startGainCommand(timeout)
        .gains
        .setTipGain(0.0)
        .gains
        .setTiltGain(0.0)
        .gains
        .setFocusGain(0.0)
        .post *> oiwfsDaytimeGains

  def defaultGains: VerifiedEpics[F, F, ApplyCommandResult] =
    sys.pwfs1
      .startGainCommand(timeout)
      .resetGain
      .post *>
      sys.pwfs2
        .startGainCommand(timeout)
        .resetGain
        .post *>
      sys.oiwfs
        .startGainCommand(timeout)
        .resetGain
        .post

  private def guideUsesPwfs1(m1Guide: M1GuideConfig, m2Guide: M2GuideConfig): Boolean =
    m1Guide.uses(M1Source.PWFS1) || m2Guide.uses(TipTiltSource.PWFS1)

  private def guideUsesPwfs2(m1Guide: M1GuideConfig, m2Guide: M2GuideConfig): Boolean =
    m1Guide.uses(M1Source.PWFS2) || m2Guide.uses(TipTiltSource.PWFS2)

  private def guideUsesOiwfs(m1Guide: M1GuideConfig, m2Guide: M2GuideConfig): Boolean =
    m1Guide.uses(M1Source.OIWFS) || m2Guide.uses(TipTiltSource.OIWFS)

  override def getGuideQuality: F[GuidersQualityValues] = (
    for {
      p1_fF <- sys.pwfs1.getQualityStatus.flux
      p1_cF <- sys.pwfs1.getQualityStatus.centroidDetected
      p2_fF <- sys.pwfs2.getQualityStatus.flux
      p2_cF <- sys.pwfs2.getQualityStatus.centroidDetected
      oi_fF <- sys.oiwfs.getQualityStatus.flux
      oi_cF <- sys.oiwfs.getQualityStatus.centroidDetected
    } yield for {
      p1_f <- p1_fF
      p1_c <- p1_cF
      p2_f <- p2_fF
      p2_c <- p2_cF
      oi_f <- oi_fF
      oi_c <- oi_cF
    } yield GuidersQualityValues(
      GuidersQualityValues.GuiderQuality(p1_f, p1_c),
      GuidersQualityValues.GuiderQuality(p2_f, p2_c),
      GuidersQualityValues.GuiderQuality(oi_f, oi_c)
    )
  ).verifiedRun(ConnectionTimeout)

  private val BafflesTimeout = FiniteDuration(40, SECONDS)

  private def setBaffles(
    central:    CentralBafflePosition,
    deployable: DeployableBafflePosition
  ): TcsCommands[F] => TcsCommands[F] = _.bafflesCommand
    .central(central)
    .bafflesCommand
    .deployable(deployable)

  override def baffles(
    central:    CentralBafflePosition,
    deployable: DeployableBafflePosition
  ): F[ApplyCommandResult] = setBaffles(central, deployable)(
    sys.tcsEpics
      .startCommand(BafflesTimeout)
  ).post
    .verifiedRun(ConnectionTimeout)

  override def getTelescopeState: F[TelescopeState] = (
    for {
      mcsfF  <- sys.mcs.getFollowingState
      scsfF  <- sys.scs.getFollowingState
      crcsfF <- sys.crcs.getFollowingState
      p1fF   <- sys.ags.status.p1Follow
      p1pF   <- sys.ags.status.p1Parked
      p2fF   <- sys.ags.status.p2Follow
      p2pF   <- sys.ags.status.p2Parked
      oifF   <- sys.ags.status.oiFollow
      oipF   <- sys.ags.status.oiParked
    } yield for {
      mcsf  <- mcsfF
      scsf  <- scsfF
      crcsf <- crcsfF
      p1f   <- p1fF
      p1p   <- p1pF
      p2f   <- p2fF
      p2p   <- p2pF
      oif   <- oifF
      oip   <- oipF
    } yield TelescopeState(
      mount = MechSystemState(NotParked, mcsf),
      scs = MechSystemState(NotParked, scsf),
      crcs = MechSystemState(NotParked, crcsf),
      pwfs1 = MechSystemState(p1p, p1f),
      pwfs2 = MechSystemState(p2p, p2f),
      oiwfs = MechSystemState(oip, oif)
    )
  ).verifiedRun(ConnectionTimeout)

  override def getTargetAdjustments: F[TargetOffsets] = (
    for {
      satrF <- sys.tcsEpics.status.sourceATargetReadout
      p1trF <- sys.tcsEpics.status.pwfs1TargetReadout
      p2trF <- sys.tcsEpics.status.pwfs2TargetReadout
      oitrF <- sys.tcsEpics.status.oiwfsTargetReadout
    } yield for {
      satr <- satrF
      p1tr <- p1trF
      p2tr <- p2trF
      oitr <- oitrF
    } yield TargetOffsets(satr.adjOffset, p1tr.adjOffset, p2tr.adjOffset, oitr.adjOffset)
  ).verifiedRun(ConnectionTimeout)

  override def getPointingCorrections: F[PointingCorrections] = (
    for {
      lcaF <- sys.tcsEpics.status.pointingCorrectionState.localCA
      lceF <- sys.tcsEpics.status.pointingCorrectionState.localCE
      gcaF <- sys.tcsEpics.status.pointingCorrectionState.guideCA
      gceF <- sys.tcsEpics.status.pointingCorrectionState.guideCE
    } yield for {
      lca <- lcaF
      lce <- lceF
      gca <- gcaF
      gce <- gceF
    } yield PointingCorrections(
      HorizontalAdjustment(lca, lce),
      HorizontalAdjustment(gca, gce)
    )
  ).verifiedRun(ConnectionTimeout)

  override def getOriginOffset: F[FocalPlaneOffset] =
    sys.tcsEpics.status.sourceATargetReadout.verifiedRun(ConnectionTimeout).map(_.originOffset)

  override def scsFollow(enable: Boolean): F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .m2FollowCommand
      .setFollow(enable)
      .post
      .verifiedRun(ConnectionTimeout)

  override def swapTarget(swapConfig: SwapConfig): F[ApplyCommandResult] =
    sys.tcsEpics.clearErrors.verifiedRun(ConnectionTimeout) *>
      disableGuide *>
      lightPath(LightSource.Sky, LightSinkName.Ac) *>
      sys.hrwfs.status.filter.verifiedRun(ConnectionTimeout).flatMap { x =>
        (resetAllTracking *>
          applyPointToGuideConfig(
            swapConfig.toTcsConfig
              .focus(_.sourceATarget)
              .andThen(Target.wavelength)
              .replace(x.getOrElse(AcFilter.Neutral).toWavelength.some)
          )(
            sys.tcsEpics.startCommand(TcsConfigTimeout)
          ).post).verifiedRun(ConnectionTimeout)
      }

  override def restoreTarget(config: TcsConfig): F[ApplyCommandResult] = {
    // TODO: consider cases where the light path should go through the AO
    val source = LightSource.Sky

    for {
      _   <- sys.tcsEpics.clearErrors.verifiedRun(ConnectionTimeout)
      _   <- disableGuide
      p1f <- sys.ags.status.pwfs1Mechs.colFilter
               .verifiedRun(ConnectionTimeout)
               .attempt
               .map(_.toOption.flatten.getOrElse(PwfsFilter.Neutral))
      p2f <- sys.ags.status.pwfs2Mechs.colFilter
               .verifiedRun(ConnectionTimeout)
               .attempt
               .map(_.toOption.flatten.getOrElse(PwfsFilter.Neutral))
      r   <- (selectOiwfs(config) *>
               VerifiedEpics.liftF(Temporal[F].sleep(OiwfsSelectionDelay)) *>
               applyTcsConfig(config, p1f, p2f)(sys.tcsEpics.startCommand(TcsConfigTimeout)).post)
               .verifiedRun(ConnectionTimeout)
      _   <- lightPath(source, config.instrument.toLightSink)
    } yield r
  }

  private def setLightPath(
    from:  LightSource,
    to:    LightSinkName,
    port:  Int,
    aoPos: AgMechPosition,
    hwPos: AgMechPosition,
    sfPos: ScienceFold
  ): TcsCommands[F] => TcsCommands[F] = (x: TcsCommands[F]) => {
    val aoFold      = (s: TcsCommands[F]) =>
      (from, aoPos) match {
        case (LightSource.AO, AgMechPosition.In) => s
        case (LightSource.AO, _)                 => s.aoFoldCommands.move.setPosition(AoFoldPosition.In)
        case (LightSource.GCAL, _)               => s
        case (_, AgMechPosition.Parked)          => s
        case _                                   => s.aoFoldCommands.park.mark
      }
    val hrwfsPickup = (s: TcsCommands[F]) =>
      (to, hwPos) match {
        case (LightSinkName.Hr, AgMechPosition.In) | (LightSinkName.Ac, AgMechPosition.In) => s
        case (LightSinkName.Hr, _) | (LightSinkName.Ac, _)                                 =>
          s.hrwfsCommands.move.setPosition(HrwfsPickupPosition.In)
        case (_, AgMechPosition.Parked)                                                    => s
        case _ if port === 1                                                               => s.hrwfsCommands.park.mark
        case _                                                                             => s
      }
    val reqPos      = ScienceFold.Position(from, to, port)
    val scienceFold = (s: TcsCommands[F]) =>
      (port, from, sfPos) match {
        case (1, LightSource.Sky, ScienceFold.Parked)        => s
        case (1, LightSource.Sky, _)                         => s.scienceFoldCommands.park.mark
        case (_, _, p: ScienceFold.Position) if p === reqPos => s
        case _                                               => s.scienceFoldCommands.move.setPosition(reqPos)
      }

    (aoFold >>> hrwfsPickup >>> scienceFold)(x)
  }

  private val LightPathTimeout = FiniteDuration(30, SECONDS)

  override def lightPath(from: LightSource, to: LightSinkName): F[ApplyCommandResult] = for {
    p2Parked <- sys.ags.status.p2Parked.verifiedRun(ConnectionTimeout).map(_ === ParkStatus.Parked)
    aoParked <- sys.ags.status.aoParked.verifiedRun(ConnectionTimeout).map(_ === ParkStatus.Parked)
    aoPos    <- aoParked.fold(AgMechPosition.Parked.pure[F],
                              sys.ags.status.aoName.verifiedRun(ConnectionTimeout)
                )
    hwParked <- sys.ags.status.hwParked.verifiedRun(ConnectionTimeout).map(_ === ParkStatus.Parked)
    hwPos    <- hwParked.fold(AgMechPosition.Parked.pure[F],
                              sys.ags.status.hwName.verifiedRun(ConnectionTimeout)
                )
    sfParked <- sys.ags.status.sfParked.verifiedRun(ConnectionTimeout).map(_ === ParkStatus.Parked)
    sfPos    <- sfParked.fold(ScienceFold.Parked.pure[F],
                              sys.ags.status.sfName.verifiedRun(ConnectionTimeout)
                )
    ports    <- getInstrumentPorts
    _        <- pwfs2Park.whenA(!p2Parked && from === LightSource.AO)
    ret      <-
      getPort(ports, to)
        .map { p =>
          setLightPath(from, to, p, aoPos, hwPos, sfPos)(
            sys.tcsEpics.startCommand(LightPathTimeout)
          ).post
            .verifiedRun(ConnectionTimeout)
        }
        .getOrElse(ApplyCommandResult.Completed.pure[F])
  } yield ret

  def getPort(instrumentPorts: InstrumentPorts, lightSinkName: LightSinkName): Option[Int] = {
    val p = lightSinkName match {
      case LightSinkName.Gmos | LightSinkName.Gmos_Ifu                             => instrumentPorts.gmosPort
      case LightSinkName.Niri_f6 | LightSinkName.Niri_f14 | LightSinkName.Niri_f32 =>
        instrumentPorts.niriPort
      case LightSinkName.Ac                                                        => 1
      case LightSinkName.Hr                                                        => 1
      case LightSinkName.Nifs                                                      => instrumentPorts.nifsPort
      case LightSinkName.Gnirs                                                     => instrumentPorts.gnirsPort
      case LightSinkName.Visitor                                                   => 0
      case LightSinkName.Flamingos2                                                => instrumentPorts.flamingos2Port
      case LightSinkName.Gsaoi                                                     => instrumentPorts.gsaoiPort
      case LightSinkName.Gpi                                                       => instrumentPorts.gpiPort
      case LightSinkName.Ghost                                                     => instrumentPorts.ghostPort
      case _                                                                       => 0
    }

    (p > 0).option(p)
  }

  private val hrwfsStream: String = "hrwfsScience"

  override def hrwfsObserve(exposureTime: TimeSpan): F[ApplyCommandResult] = (
    sys.hrwfs
      .startCommand(timeout)
      .setExposureTime(exposureTime.toSeconds.toDouble)
      .setNumberOfFrames(-1)
      .setQuicklookStream(hrwfsStream)
      .setDhsOption(2)
      .post *>
      sys.hrwfs
        .startCommand(timeout)
        .setDhsLabel("NONE")
        .post
  ).verifiedRun(ConnectionTimeout)

  override def hrwfsStopObserve: F[ApplyCommandResult] =
    sys.hrwfs
      .startCommand(timeout)
      .stop
      .post
      .verifiedRun(ConnectionTimeout)

  private val M1ParkTimeout = FiniteDuration(30, SECONDS)

  override def m1Park: F[ApplyCommandResult] =
    sys.tcsEpics.startCommand(M1ParkTimeout).m1Commands.park.post.verifiedRun(ConnectionTimeout)

  override def m1Unpark: F[ApplyCommandResult] =
    sys.tcsEpics.startCommand(M1ParkTimeout).m1Commands.unpark.post.verifiedRun(ConnectionTimeout)

  override def m1UpdateOn: F[ApplyCommandResult] = sys.tcsEpics
    .startCommand(timeout)
    .m1Commands
    .ao(true)
    .m1Commands
    .figureUpdates(true)
    .post
    .verifiedRun(ConnectionTimeout)

  override def m1UpdateOff: F[ApplyCommandResult] =
    sys.tcsEpics.startCommand(timeout).m1Commands.ao(false).post.verifiedRun(ConnectionTimeout)

  override def m1ZeroFigure: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(M1ParkTimeout)
      .m1Commands
      .zero("FIGURE")
      .post
      .verifiedRun(ConnectionTimeout)

  override def m1LoadAoFigure: F[ApplyCommandResult] = sys.tcsEpics
    .startCommand(timeout)
    .m1Commands
    .loadModel("AO")
    .post
    .verifiedRun(ConnectionTimeout)

  override def m1LoadNonAoFigure: F[ApplyCommandResult] = sys.tcsEpics
    .startCommand(timeout)
    .m1Commands
    .loadModel("non-AO")
    .post
    .verifiedRun(ConnectionTimeout)

  val SettleTime: FiniteDuration = FiniteDuration.apply(1, TimeUnit.SECONDS)

  private def rectToPolar(x: Angle, y: Angle): (Angle, Angle) = {
    val size: Angle  = Angle.fromDoubleRadians(
      Math.sqrt(
        Math.pow(x.toSignedDoubleRadians, 2.0)
          + Math.pow(y.toSignedDoubleRadians, 2.0)
      )
    )
    val angle: Angle = Angle.fromDoubleRadians(
      Math.atan2(y.toSignedDoubleRadians, x.toSignedDoubleRadians)
    )

    (size, angle)
  }

  private def applyAcquisitionAdj(
    offset: Offset,
    ipa:    Option[Angle],
    iaa:    Option[Angle]
  ): F[ApplyCommandResult] = {
    val (s, angle) = rectToPolar(-offset.q.toAngle, offset.p.toAngle)
    val size       = Angle.signedDecimalArcseconds.get(s).doubleValue
    val adjTimeout =
      if (ipa.isDefined && iaa.isDefined) List(RotMoveTimeout, offsetTimeout(size)).max
      else offsetTimeout(size)

    (ipa, iaa)
      .mapN { (ip, ia) =>
        sys.tcsEpics.startCommand(RotMoveTimeout).rotatorCommand.ipa(ip).rotatorCommand.iaa(ia).post
      }
      .getOrElse(VerifiedEpics.pureF(ApplyCommandResult.Completed))
      .verifiedRun(ConnectionTimeout) *>
      (if (Math.abs(size) > 1e-6) {
         sys.tcsEpics
           .startCommand(CommandAcknowledgeTimeout)
           .originAdjustCommand
           .frame(ReferenceFrame.Instrument)
           .originAdjustCommand
           .size(size)
           .originAdjustCommand
           .angle(angle)
           .originAdjustCommand
           .vtMask(
             List(VirtualTelescope.SourceA, VirtualTelescope.SourceB, VirtualTelescope.SourceC)
           )
           .post
       } else VerifiedEpics.pureF(ApplyCommandResult.Completed)).verifiedRun(ConnectionTimeout) <*
      (if (Math.abs(size) > 1e-6 || (ipa.isDefined && iaa.isDefined))
         sys.tcsEpics.status.waitInPosition(SettleTime, adjTimeout)
       else VerifiedEpics.unit[F, F]).verifiedRun(ConnectionTimeout)

  }

  private def shouldPauseGuide(offset: Offset, ipa: Option[Angle], iaa: Option[Angle]): Boolean =
    (ipa.isDefined && iaa.isDefined) ||
      offset.p.toAngle =!= Angle.Angle0 ||
      offset.q.toAngle =!= Angle.Angle0

  override def acquisitionAdj(offset: Offset, ipa: Option[Angle], iaa: Option[Angle])(
    guide: GuideConfig
  ): F[ApplyCommandResult] =
    getGuideState.flatMap { gs =>
      Logger[F].debug(
        (gs.isGuiding && shouldPauseGuide(offset, ipa, iaa)).fold("P", "Not p") +
          s"ausing loops because isGuiding = ${gs.isGuiding} and requirePause = ${shouldPauseGuide(offset, ipa, iaa)}"
      ) *>
        pauseGuide.whenA(gs.isGuiding && shouldPauseGuide(offset, ipa, iaa)) *>
        applyAcquisitionAdj(offset, ipa, iaa) <*
        (
          Logger[F].debug(
            (guide.tcsGuide.isGuiding && (!gs.isGuiding || shouldPauseGuide(offset, ipa, iaa)))
              .fold("R", "Not r") +
              s"esuming loops because requestedGuide = ${guide.tcsGuide.isGuiding} and wasNotGuiding = ${!gs.isGuiding} or hadToPause = ${shouldPauseGuide(offset, ipa, iaa)}"
          ) *>
            resumeGuide(guide.tcsGuide)
              .whenA(
                guide.tcsGuide.isGuiding && (!gs.isGuiding || shouldPauseGuide(offset, ipa, iaa))
              )
        )
    }

  private def wfsRefAdjustParams(
    st:  PwfsAngles[F],
    pol: (Angle, Angle)
  ): F[(ReferenceFrame, Double, Angle)] = (
    for {
      tableAngleF <- st.tableAngle
      armAngleF   <- st.armAngle
    } yield for {
      tableAngle <- tableAngleF
      armAngle   <- armAngleF
    } yield (ReferenceFrame.XY,
             Angle.signedDecimalArcseconds.get(pol._1).doubleValue,
             pol._2 + tableAngle + armAngle
    )
  ).verifiedRun(ConnectionTimeout)

  private def adjustParams(
    handsetAdjustment: HandsetAdjustment
  ): F[(ReferenceFrame, Double, Angle)] =
    handsetAdjustment match {
      case HandsetAdjustment.EquatorialAdjustment(deltaRA, deltaDec)                         =>
        val pol = rectToPolar(deltaDec, deltaRA)
        (ReferenceFrame.Tracking, Angle.signedDecimalArcseconds.get(pol._1).doubleValue, pol._2)
          .pure[F]
      case HandsetAdjustment.FocalPlaneAdjustment(value)                                     =>
        val pol = rectToPolar(value.deltaY.value, -value.deltaX.value)
        (ReferenceFrame.XY, Angle.signedDecimalArcseconds.get(pol._1).doubleValue, pol._2).pure[F]
      case HandsetAdjustment.HorizontalAdjustment(deltaAz, deltaEl)                          =>
        val pol = rectToPolar(deltaEl, deltaAz)
        (ReferenceFrame.AzimuthElevation,
         Angle.signedDecimalArcseconds.get(pol._1).doubleValue,
         pol._2
        )
          .pure[F]
      case HandsetAdjustment.InstrumentAdjustment(value)                                     =>
        val pol = rectToPolar(-value.q.toAngle, value.p.toAngle)
        (ReferenceFrame.Instrument, Angle.signedDecimalArcseconds.get(pol._1).doubleValue, pol._2)
          .pure[F]
      case HandsetAdjustment.ProbeFrameAdjustment(probeRefFrame, deltaX, deltaY, alignAngle) =>
        probeRefFrame match {
          case GuideProbe.PWFS1           =>
            wfsRefAdjustParams(sys.ags.status.pwfs1Angles, rectToPolar(-deltaX, -deltaY))
          case GuideProbe.PWFS2           =>
            wfsRefAdjustParams(sys.ags.status.pwfs2Angles, rectToPolar(deltaX, deltaY))
          case GuideProbe.GmosOIWFS       =>
            val pol = rectToPolar(deltaY, deltaX)
            (ReferenceFrame.XY,
             Angle.signedDecimalArcseconds.get(pol._1).doubleValue,
             pol._2 + alignAngle.getOrElse(Angle.Angle0)
            ).pure[F]
          case GuideProbe.Flamingos2OIWFS =>
            val pol = rectToPolar(deltaY, deltaX)
            (ReferenceFrame.XY,
             Angle.signedDecimalArcseconds.get(pol._1).doubleValue,
             pol._2 + alignAngle.getOrElse(Angle.Angle0)
            ).pure[F]
        }
    }

  private val AdjTimeout = FiniteDuration(20, SECONDS)

  override def targetAdjust(
    target:            VirtualTelescope,
    handsetAdjustment: HandsetAdjustment,
    openLoops:         Boolean
  )(guide: GuideConfig): F[ApplyCommandResult] =
    pauseGuide.whenA(openLoops) *>
      adjustParams(handsetAdjustment).flatMap { case (frame, size, angle) =>
        sys.tcsEpics
          .startCommand(AdjTimeout)
          .targetAdjustCommand
          .frame(frame)
          .targetAdjustCommand
          .size(size)
          .targetAdjustCommand
          .angle(angle)
          .targetAdjustCommand
          .vtMask(List(target))
          .post
          .verifiedRun(ConnectionTimeout) <*
          sys.tcsEpics.status
            .waitInPosition(SettleTime, offsetTimeout(size))
            .verifiedRun(ConnectionTimeout)
      } <*
      resumeGuide(guide.tcsGuide).whenA(openLoops)

  override def originAdjust(handsetAdjustment: HandsetAdjustment, openLoops: Boolean)(
    guide: GuideConfig
  ): F[ApplyCommandResult] =
    pauseGuide.whenA(openLoops) *>
      adjustParams(handsetAdjustment).flatMap { case (frame, size, angle) =>
        sys.tcsEpics
          .startCommand(AdjTimeout)
          .originAdjustCommand
          .frame(frame)
          .originAdjustCommand
          .size(size)
          .originAdjustCommand
          .angle(angle)
          .originAdjustCommand
          .vtMask(
            List(VirtualTelescope.SourceA, VirtualTelescope.SourceB, VirtualTelescope.SourceC)
          )
          .post
          .verifiedRun(ConnectionTimeout) <*
          sys.tcsEpics.status
            .waitInPosition(SettleTime, offsetTimeout(size))
            .verifiedRun(ConnectionTimeout)
      } <*
      resumeGuide(guide.tcsGuide).whenA(openLoops)

  override def pointingAdjust(handsetAdjustment: HandsetAdjustment): F[ApplyCommandResult] =
    adjustParams(handsetAdjustment).flatMap { case (frame, size, angle) =>
      sys.tcsEpics
        .startCommand(AdjTimeout)
        .pointingAdjustCommand
        .frame(frame)
        .pointingAdjustCommand
        .size(size)
        .pointingAdjustCommand
        .angle(angle)
        .post
        .verifiedRun(ConnectionTimeout)
    }

  override def targetOffsetAbsorb(target: VirtualTelescope): F[ApplyCommandResult] = {
    val selection: OffsetIndexSelection = target match {
      case VirtualTelescope.Mount | VirtualTelescope.SourceA | VirtualTelescope.SourceB |
          VirtualTelescope.SourceC =>
        OffsetIndexSelection.All
      case _ => OffsetIndexSelection.Index(0)
    }
    sys.tcsEpics
      .startCommand(timeout)
      .targetOffsetAbsorb
      .vt(target)
      .targetOffsetAbsorb
      .index(selection)
      .post
      .verifiedRun(ConnectionTimeout)
  }

  private val MaxClearedOffset: Double = 120.0 // arcsec

  override def targetOffsetClear(target: VirtualTelescope, openLoops: Boolean)(
    guide: GuideConfig
  ): F[ApplyCommandResult] =
    pauseGuide.whenA(openLoops) *>
      sys.tcsEpics
        .startCommand(AdjTimeout)
        .targetOffsetClear
        .vt(target)
        .targetOffsetClear
        .index(OffsetIndexSelection.All)
        .post
        .verifiedRun(ConnectionTimeout) <*
      sys.tcsEpics.status
        .waitInPosition(SettleTime, offsetTimeout(MaxClearedOffset))
        .verifiedRun(ConnectionTimeout) <*
      resumeGuide(guide.tcsGuide).whenA(openLoops)

  override def originOffsetAbsorb: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(timeout)
      .originOffsetAbsorb
      .vt(VirtualTelescope.SourceA)
      .originOffsetAbsorb
      .index(OffsetIndexSelection.All)
      .post
      .verifiedRun(ConnectionTimeout) *>
      sys.tcsEpics
        .startCommand(timeout)
        .originOffsetAbsorb
        .vt(VirtualTelescope.SourceB)
        .originOffsetAbsorb
        .index(OffsetIndexSelection.All)
        .post
        .verifiedRun(ConnectionTimeout) *>
      sys.tcsEpics
        .startCommand(timeout)
        .originOffsetAbsorb
        .vt(VirtualTelescope.SourceC)
        .originOffsetAbsorb
        .index(OffsetIndexSelection.All)
        .post
        .verifiedRun(ConnectionTimeout)

  override def originOffsetClear(openLoops: Boolean)(guide: GuideConfig): F[ApplyCommandResult] =
    pauseGuide.whenA(openLoops) *>
      sys.tcsEpics
        .startCommand(AdjTimeout)
        .originOffsetClear
        .vt(VirtualTelescope.SourceA)
        .originOffsetClear
        .index(OffsetIndexSelection.All)
        .post
        .verifiedRun(ConnectionTimeout) *>
      sys.tcsEpics
        .startCommand(timeout)
        .originOffsetClear
        .vt(VirtualTelescope.SourceB)
        .originOffsetClear
        .index(OffsetIndexSelection.All)
        .post
        .verifiedRun(ConnectionTimeout) *>
      sys.tcsEpics
        .startCommand(timeout)
        .originOffsetClear
        .vt(VirtualTelescope.SourceC)
        .originOffsetClear
        .index(OffsetIndexSelection.All)
        .post
        .verifiedRun(ConnectionTimeout) <*
      sys.tcsEpics.status
        .waitInPosition(SettleTime, offsetTimeout(MaxClearedOffset))
        .verifiedRun(ConnectionTimeout) <*
      resumeGuide(guide.tcsGuide).whenA(openLoops)

  override def pointingOffsetClearLocal: F[ApplyCommandResult] =
    sys.tcsEpics
      .startCommand(AdjTimeout)
      .pointingConfigCommand
      .name(PointingParameter.CA)
      .pointingConfigCommand
      .level(PointingConfigLevel.Local)
      .pointingConfigCommand
      .value(0.0)
      .post
      .verifiedRun(ConnectionTimeout) *>
      sys.tcsEpics
        .startCommand(AdjTimeout)
        .pointingConfigCommand
        .name(PointingParameter.CE)
        .pointingConfigCommand
        .level(PointingConfigLevel.Local)
        .pointingConfigCommand
        .value(0.0)
        .post
        .verifiedRun(ConnectionTimeout)

  override def pointingOffsetAbsorbGuide: F[ApplyCommandResult] =
    sys.tcsEpics.startCommand(timeout).absorbGuideCommand.mark.post.verifiedRun(ConnectionTimeout)

  override def pointingOffsetClearGuide: F[ApplyCommandResult] =
    sys.tcsEpics.startCommand(AdjTimeout).zeroGuideCommand.mark.post.verifiedRun(ConnectionTimeout)

  private val AcMechTimeout              = FiniteDuration(60, SECONDS)
  override val acCommands: AcCommands[F] = new AcCommands[F] {
    override def lens(l: AcLens): F[ApplyCommandResult] =
      sys.hrwfs.startCommand(AcMechTimeout).setLens(l).post.verifiedRun(ConnectionTimeout)

    override def ndFilter(ndFilter: AcNdFilter): F[ApplyCommandResult] = sys.hrwfs
      .startCommand(AcMechTimeout)
      .setNdFilter(ndFilter)
      .post
      .verifiedRun(ConnectionTimeout)

    override def filter(filter: AcFilter): F[ApplyCommandResult] =
      sys.hrwfs.startCommand(AcMechTimeout).setColFilter(filter).post.verifiedRun(ConnectionTimeout)

    private def setWindowSize(size: AcWindow): F[ApplyCommandResult] = {
      val cmd = sys.hrwfs.startCommand(AcMechTimeout)

      (size match {
        case AcWindow.Full            => cmd.enableWindow(0).setBinning(0)
        case AcWindow.Square100(x, y) =>
          cmd
            .enableWindow(1)
            .setBinning(0)
            .setWindowHeight(100)
            .setWindowWidth(100)
            .setWindowX(x)
            .setWindowY(y)
        case AcWindow.Square200(x, y) =>
          cmd
            .enableWindow(1)
            .setBinning(0)
            .setWindowHeight(200)
            .setWindowWidth(200)
            .setWindowX(x)
            .setWindowY(y)
      }).post.verifiedRun(ConnectionTimeout)
    }

    override def windowSize(size: AcWindow): F[ApplyCommandResult] =
      sys.hrwfs.status.observe.verifiedRun(ConnectionTimeout).flatMap { active =>
        hrwfsStopObserve.whenA(active === CarState.BUSY) *>
          setWindowSize(size) <*
          sys.hrwfs
            .startCommand(timeout)
            .setDhsLabel("NONE")
            .post
            .verifiedRun(ConnectionTimeout)
            .whenA(active === CarState.BUSY)
      }

    override def getState: F[AcMechsState] = (for {
      lnF <- sys.hrwfs.status.lens
      ndF <- sys.hrwfs.status.ndFilter
      flF <- sys.hrwfs.status.filter
    } yield for {
      ln <- lnF
      nd <- ndF
      fl <- flF
    } yield AcMechsState(ln, nd, fl)).verifiedRun(ConnectionTimeout)

  }

  private val PwfsMechTimeout = FiniteDuration(20, SECONDS)

  private def buildPwfsMechanismCommands(
    l: Getter[TcsCommands[F], PwfsMechCommands[F]],
    w: Getter[TcsCommands[F], WavelengthCommand[F, TcsCommands[F]]]
  ): PwfsMechanismCommands[F] = new PwfsMechanismCommands[F] {

    override def filter(f: PwfsFilter): F[ApplyCommandResult] = {
      val filterAction = l
        .get(sys.tcsEpics.startCommand(PwfsMechTimeout))
        .filter(f)

      w.get(filterAction)
        .wavelength(f.wavel)
        .post
        .verifiedRun(ConnectionTimeout)
    }

    override def fieldStop(fs: PwfsFieldStop): F[ApplyCommandResult] = l
      .get(sys.tcsEpics.startCommand(PwfsMechTimeout))
      .fieldStop(fs)
      .post
      .verifiedRun(ConnectionTimeout)
  }

  override val pwfs1Mechs: PwfsMechanismCommands[F] = buildPwfsMechanismCommands(
    Getter[TcsCommands[F], PwfsMechCommands[F]](_.pwfs1MechCommands),
    Getter[TcsCommands[F], WavelengthCommand[F, TcsCommands[F]]](_.pwfs1Wavel)
  )

  override val pwfs2Mechs: PwfsMechanismCommands[F] = buildPwfsMechanismCommands(
    Getter[TcsCommands[F], PwfsMechCommands[F]](_.pwfs2MechCommands),
    Getter[TcsCommands[F], WavelengthCommand[F, TcsCommands[F]]](_.pwfs2Wavel)
  )

  private def getPwfsMechs(c: AgsEpicsSystem.PwfsMechs[F]): F[PwfsMechsState] = (
    for {
      flF <- c.colFilter
      fsF <- c.fieldStop
    } yield for {
      fl <- flF
      fs <- fsF
    } yield PwfsMechsState(fl, fs)
  ).verifiedRun(ConnectionTimeout)

  override def getPwfs1Mechs: F[PwfsMechsState] = getPwfsMechs(sys.ags.status.pwfs1Mechs)

  override def getPwfs2Mechs: F[PwfsMechsState] = getPwfsMechs(sys.ags.status.pwfs2Mechs)

  override def getBaffles: F[BafflesState] = (for {
    ctF <- sys.scs.getCentralBaffleState
    dpF <- sys.scs.getDeployableBaffleState
  } yield for {
    ct <- ctF
    dp <- dpF
  } yield BafflesState(ct, dp)).verifiedRun(ConnectionTimeout)

  protected def wfsCircularBuffer(
    ctr:    CircularBufferControl[F],
    enable: Boolean
  ): F[ApplyCommandResult] = ctr.circularBufferStatus
    .flatMap { x =>
      VerifiedEpics.ifF(x.map(_.imageEnabled === enable))(
        VerifiedEpics.pureF(ApplyCommandResult.Completed)
      )(
        ctr
          .startCircularBufferCommand(timeout)
          .setEnabled(enable)
          .post
      )
    }
    .verifiedRun(ConnectionTimeout)

  override def pwfs1CircularBuffer(enable: Boolean): F[ApplyCommandResult] =
    wfsCircularBuffer(sys.pwfs1, enable)

  override def pwfs2CircularBuffer(enable: Boolean): F[ApplyCommandResult] =
    wfsCircularBuffer(sys.pwfs2, enable)

  protected def getWfsConfig(
    wfs: WfsEpicsSystem[F],
    cb:  CircularBufferControl[F]
  ): F[WfsConfiguration] = (
    for {
      itF <- wfs.getIntegrationTime
      svF <- cb.circularBufferStatus.map(_.map(_.imageEnabled))
    } yield for {
      it <- itF
      sv <- svF
    } yield WfsConfiguration(it, sv)
  ).verifiedRun(ConnectionTimeout)

  override def getPwfs1Config: F[WfsConfiguration] = getWfsConfig(sys.pwfs1, sys.pwfs1)

  override def getPwfs2Config: F[WfsConfiguration] = getWfsConfig(sys.pwfs2, sys.pwfs2)

  protected def wfsConfigStream(
    wfsSys: WfsEpicsSystem[F],
    cbSys:  CircularBufferControl[F]
  ): F[Stream[F, WfsConfiguration]] = {
    val startVal: VerifiedEpics[F, F, (Option[TimeSpan], Option[Boolean])] = for {
      exp <- wfsSys.getIntegrationTime
      sav <- cbSys.circularBufferStatus.map(_.map(_.imageEnabled))
    } yield (exp.attempt.map(_.toOption), sav.attempt.map(_.toOption)).mapN((_, _))

    val streamsRes: VerifiedEpics[F, Resource[F, *], (Stream[F, TimeSpan], Stream[F, Boolean])] =
      for {
        expS <- wfsSys.integrationTimeStream
        savS <- cbSys.imgCircularBufferStrean
      } yield (expS, savS).mapN((_, _))

    val streams: VerifiedEpics[F, Resource[F, *], Stream[F, (Option[TimeSpan], Option[Boolean])]] =
      streamsRes.map(_.map { (a, b) =>
        Stream(a.map(x => (x.some, none[Boolean])),
               b.map(x => (none[TimeSpan], x.some))
        ).parJoinUnbounded
      })

    (
      for {
        v0  <- startVal
        ssr <- streams
      } yield ssr.use(ss =>
        v0.map(
          ss.scan(_) { (current, update) =>
            update match {
              case (Some(a), None) => (Some(a), update._2)
              case (None, Some(b)) => (current._1, Some(b))
              case _               => current // Should not happen with this mapping
            }
          }.map {
            _.mapN(WfsConfiguration(_, _))
          }.flattenOption
        )
      )
    ).verifiedRun(ConnectionTimeout)
  }

  def pwfs1ConfigStream: F[Stream[F, WfsConfiguration]] = wfsConfigStream(
    sys.pwfs1,
    sys.pwfs1
  )

  def pwfs2ConfigStream: F[Stream[F, WfsConfiguration]] = wfsConfigStream(
    sys.pwfs2,
    sys.pwfs2
  )

}

object TcsBaseControllerEpics {

  def encodeOiwfsSelect(oiGuideConfig: Option[GuiderConfig], instrument: Instrument): String =
    oiGuideConfig
      .flatMap { _ =>
        instrument match
          case Instrument.GmosNorth | Instrument.GmosSouth => "GMOS".some
          case Instrument.Gnirs                            => "GNIRS".some
          case Instrument.Niri                             => "NIRI".some
          case Instrument.Flamingos2                       => "F2".some
          case _                                           => None
      }
      .getOrElse("None")

  case class WfsConfigState(
    period:          Option[TimeSpan],
    configuredForQl: Option[Boolean]
  )

  case class State(
    pwfs1: WfsConfigState,
    pwfs2: WfsConfigState,
    oiwfs: WfsConfigState
  )

  object State {
    val default: State = State(
      WfsConfigState(None, None),
      WfsConfigState(None, None),
      WfsConfigState(None, None)
    )
  }

  case class WfsGuideStates(
    pwfs1: TrackingConfig,
    pwfs2: TrackingConfig,
    oiwfs: TrackingConfig
  )

  extension (x: TelescopeGuideConfig) {
    def isGuiding: Boolean =
      x.m1Guide =!= M1GuideConfig.M1GuideOff || x.m2Guide =!= M2GuideConfig.M2GuideOff || x.probeGuide.isDefined
  }

  extension (x: GuideState) {
    def isGuiding: Boolean =
      x.m1Guide =!= M1GuideConfig.M1GuideOff || x.m2Guide =!= M2GuideConfig.M2GuideOff
  }

  // For commands that complete immediately, this is the only timeout
  private val CommandAcknowledgeTimeout: FiniteDuration = 10.seconds
  // Timeout rate for offsets
  private val OffsetTimeout: Double                     = 1.0 // seconds/arcsec

  private def offsetTimeout(size: Double): FiniteDuration =
    CommandAcknowledgeTimeout + (size * OffsetTimeout).seconds

}
