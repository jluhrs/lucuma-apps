// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Applicative
import cats.Monad
import cats.Parallel
import cats.effect.Async
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.enums.GuideProbe
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.ProbeGuide
import lucuma.core.util.Enumerated
import mouse.all.*
import navigate.epics.Channel
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.model.Distance
import navigate.model.FocalPlaneOffset
import navigate.model.RotatorAngle
import navigate.model.RotatorAngle.*
import navigate.model.ShortcircuitTargetFilter
import navigate.model.enums.AoFoldPosition
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.DomeMode
import navigate.model.enums.HrwfsPickupPosition
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server.ApplyCommandResult
import navigate.server.acm.CadDirective
import navigate.server.acm.Encoder
import navigate.server.acm.Encoder.given
import navigate.server.acm.GeminiApplyCommand
import navigate.server.acm.ObserveCommand
import navigate.server.acm.ParameterList.*
import navigate.server.acm.writeCadParam
import navigate.server.epicsdata.BinaryOnOff
import navigate.server.epicsdata.BinaryOnOff.given
import navigate.server.epicsdata.BinaryOnOffCapitalized
import navigate.server.epicsdata.BinaryYesNo
import navigate.server.epicsdata.BinaryYesNo.given
import navigate.server.epicsdata.NodState
import navigate.server.tcs.TcsEpicsSystem.TcsStatus

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.MILLISECONDS
import scala.concurrent.duration.FiniteDuration

import encoders.given
import ScienceFoldPositionCodex.given
import TcsChannels.{
  AgMechChannels,
  ProbeChannels,
  ProbeTrackingChannels,
  ProbeTrackingStateChannels,
  PwfsMechCmdChannels,
  SlewChannels,
  TargetChannels,
  WfsChannels
}

trait TcsEpicsSystem[F[_]] {
  // TcsCommands accumulates the list of channels that need to be written to set parameters.
  // Once all the parameters are defined, the user calls the post method. Only then the EPICS channels will be verified,
  // the parameters written, and the apply record triggered.
  def startCommand(timeout: FiniteDuration): TcsEpicsSystem.TcsCommands[F]

  def startOiwfsCommand(timeout: FiniteDuration): TcsEpicsSystem.WfsCommands[F]

  def startPwfs1Command(timeout: FiniteDuration): TcsEpicsSystem.WfsCommands[F]

  def startPwfs2Command(timeout: FiniteDuration): TcsEpicsSystem.WfsCommands[F]

  def clearErrors: VerifiedEpics[F, F, Unit]

  val status: TcsStatus[F]
}

object TcsEpicsSystem {

  trait TcsEpics[F[_]] {
    def post(timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult]
    def clear: VerifiedEpics[F, F, Unit]

    val mountParkCmd: ParameterlessCommandChannels[F]
    val mountFollowCmd: Command1Channels[F, BinaryOnOff]
    val rotStopCmd: Command1Channels[F, BinaryYesNo]
    val rotParkCmd: ParameterlessCommandChannels[F]
    val rotFollowCmd: Command1Channels[F, BinaryOnOff]
    val rotMoveCmd: Command1Channels[F, Double]
    val carouselModeCmd: Command5Channels[F, String, String, Double, BinaryOnOff, BinaryOnOff]
    val carouselMoveCmd: Command1Channels[F, Double]
    val shuttersMoveCmd: Command2Channels[F, Double, Double]
    val ventGatesMoveCmd: Command2Channels[F, Double, Double]
    val slewCmd: SlewCommandChannels[F]
    val rotatorConfigCmd: Command4Channels[F, Double, String, String, Double]
    val originCmd: Command6Channels[F, Double, Double, Double, Double, Double, Double]
    val focusOffsetCmd: Command1Channels[F, Double]
    val oiwfsProbeTrackingCmd: ProbeTrackingCommandChannels[F]
    val oiwfsProbeCmds: ProbeCommandsChannels[F]
    val m1GuideConfigCmd: Command4Channels[F, String, String, Int, String]
    val m1GuideCmd: Command1Channels[F, BinaryOnOff]
    val m2GuideCmd: Command1Channels[F, BinaryOnOff]
    val m2GuideModeCmd: Command1Channels[F, BinaryOnOff]
    val m2GuideConfigCmd: Command7Channels[F,
                                           String,
                                           Double,
                                           String,
                                           Option[Double],
                                           Option[Double],
                                           String,
                                           BinaryOnOff
    ]
    val m2GuideResetCmd: ParameterlessCommandChannels[F]
    val m2FollowCmd: Command1Channels[F, BinaryOnOff]
    val mountGuideCmd: Command4Channels[F, BinaryOnOff, String, Double, Double]
    val probeGuideModeCmd: Command3Channels[F, BinaryOnOff, GuideProbe, GuideProbe]
    val oiwfsSelectCmd: Command2Channels[F, String, String]
    val bafflesCmd: Command2Channels[F, CentralBafflePosition, DeployableBafflePosition]

    // val offsetACmd: OffsetCmd[F]
    // val offsetBCmd: OffsetCmd[F]
    // val wavelSourceB: TargetWavelengthCmd[F]
    // val m2Beam: M2Beam[F]
    val hrwfsCmds: AgMechCommandsChannels[F, HrwfsPickupPosition]
    val scienceFoldCmds: AgMechCommandsChannels[F, ScienceFold.Position]
    val aoFoldCmds: AgMechCommandsChannels[F, AoFoldPosition]
    val m1Cmds: M1CommandsChannels[F]
    // val observe: EpicsCommand[F]
    // val endObserve: EpicsCommand[F]
    // // GeMS Commands
    // import VirtualGemsTelescope._
    // val g1ProbeGuideCmd: ProbeGuideCmd[F]
    // val g2ProbeGuideCmd: ProbeGuideCmd[F]
    // val g3ProbeGuideCmd: ProbeGuideCmd[F]
    // val g4ProbeGuideCmd: ProbeGuideCmd[F]
    // def gemsProbeGuideCmd(g: VirtualGemsTelescope): ProbeGuideCmd[F] = g match {
    //   case G1 => g1ProbeGuideCmd
    //   case G2 => g2ProbeGuideCmd
    //   case G3 => g3ProbeGuideCmd
    //   case G4 => g4ProbeGuideCmd
    // }
    // val wavelG1: TargetWavelengthCmd[F]
    // val wavelG2: TargetWavelengthCmd[F]
    // val wavelG3: TargetWavelengthCmd[F]
    // val wavelG4: TargetWavelengthCmd[F]
    // def gemsWavelengthCmd(g: VirtualGemsTelescope): TargetWavelengthCmd[F] = g match {
    //   case G1 => wavelG1
    //   case G2 => wavelG2
    //   case G3 => wavelG3
    //   case G4 => wavelG4
    // }
    // val cwfs1ProbeFollowCmd: ProbeFollowCmd[F]
    // val cwfs2ProbeFollowCmd: ProbeFollowCmd[F]
    // val cwfs3ProbeFollowCmd: ProbeFollowCmd[F]
    // val odgw1FollowCmd: ProbeFollowCmd[F]
    // val odgw2FollowCmd: ProbeFollowCmd[F]
    // val odgw3FollowCmd: ProbeFollowCmd[F]
    // val odgw4FollowCmd: ProbeFollowCmd[F]
    // val odgw1ParkCmd: EpicsCommand[F]
    // val odgw2ParkCmd: EpicsCommand[F]
    // val odgw3ParkCmd: EpicsCommand[F]
    // val odgw4ParkCmd: EpicsCommand[F]
  }

  trait TcsStatus[F[_]] {
    // val aoCorrect: AoCorrect[F]
    // val aoPrepareControlMatrix: AoPrepareControlMatrix[F]
    // val aoFlatten: EpicsCommand[F]
    // val aoStatistics: AoStatistics[F]
    // val targetFilter: TargetFilter[F]
    def absorbTipTilt: VerifiedEpics[F, F, Int]
    def m1GuideSource: VerifiedEpics[F, F, String]
    def m1Guide: VerifiedEpics[F, F, BinaryOnOff]
    def m2p1Guide: VerifiedEpics[F, F, String]
    def m2p2Guide: VerifiedEpics[F, F, String]
    def m2oiGuide: VerifiedEpics[F, F, String]
    def m2aoGuide: VerifiedEpics[F, F, String]
    def comaCorrect: VerifiedEpics[F, F, BinaryOnOff]
    def m2GuideState: VerifiedEpics[F, F, BinaryOnOff]
    // def xoffsetPoA1: F[Double]
    // def yoffsetPoA1: F[Double]
    // def xoffsetPoB1: F[Double]
    // def yoffsetPoB1: F[Double]
    // def xoffsetPoC1: F[Double]
    // def yoffsetPoC1: F[Double]
    // def sourceAWavelength: F[Double]
    // def sourceBWavelength: F[Double]
    // def sourceCWavelength: F[Double]
    // def chopBeam: F[String]
    // def aoFollowS: F[String]
    // def oiName: F[String]
    def pwfs1On: VerifiedEpics[F, F, BinaryYesNo]
    def pwfs2On: VerifiedEpics[F, F, BinaryYesNo]
    def oiwfsOn: VerifiedEpics[F, F, BinaryYesNo]
    def nodState: VerifiedEpics[F, F, NodState]
    // def instrAA: F[Double]
    // def inPosition: F[String]
    // def agInPosition: F[Double]
    val pwfs1ProbeGuideState: ProbeGuideState[F]
    val pwfs2ProbeGuideState: ProbeGuideState[F]
    val oiwfsProbeGuideState: ProbeGuideState[F]
    // // This functions returns a F that, when run, first waits tcsSettleTime to absorb in-position transients, then waits
    // // for the in-position to change to true and stay true for stabilizationTime. It will wait up to `timeout`
    // // seconds for that to happen.
    def waitInPosition(
      stabilizationTime: FiniteDuration,
      timeout:           FiniteDuration
    ): VerifiedEpics[F, F, Unit]
    def waitPwfs1Sky(timeout: FiniteDuration): VerifiedEpics[F, F, Unit]
    def waitPwfs2Sky(timeout: FiniteDuration): VerifiedEpics[F, F, Unit]
    def waitOiwfsSky(timeout: FiniteDuration): VerifiedEpics[F, F, Unit]
    def probeGuideEnabled: VerifiedEpics[F, F, BinaryOnOff]
    def probeGuideSource: VerifiedEpics[F, F, String]
    def probeGuideSink: VerifiedEpics[F, F, String]
    // // `waitAGInPosition` works like `waitInPosition`, but for the AG in-position flag.
    // /* TODO: AG inposition can take up to 1[s] to react to a TCS command. If the value is read before that, it may induce
    //   * an error. A better solution is to detect the edge, from not in position to in-position.
    //   */
    // def waitAGInPosition(timeout: FiniteDuration)(using T: Timer[F]): F[Unit]
    // def hourAngle: F[String]
    // def localTime: F[String]
    // def trackingFrame: F[String]
    // def trackingEpoch: F[Double]
    // def equinox: F[Double]
    // def trackingEquinox: F[String]
    // def trackingDec: F[Double]
    // def trackingRA: F[Double]
    // def elevation: F[Double]
    // def azimuth: F[Double]
    // def crPositionAngle: F[Double]
    // def ut: F[String]
    // def date: F[String]
    // def m2Baffle: F[String]
    // def m2CentralBaffle: F[String]
    // def st: F[String]
    // def sfRotation: F[Double]
    // def sfTilt: F[Double]
    // def sfLinear: F[Double]
    // def instrPA: F[Double]
    def sourceATargetReadout: VerifiedEpics[F, F, TargetData]
    def pwfs1TargetReadout: VerifiedEpics[F, F, TargetData]
    def pwfs2TargetReadout: VerifiedEpics[F, F, TargetData]
    def oiwfsTargetReadout: VerifiedEpics[F, F, TargetData]
    val pointingCorrectionState: PointingCorrectionState[F]
    // def aoFoldPosition: F[String]
    // def useAo: F[BinaryYesNo]
    // def airmass: F[Double]
    // def airmassStart: F[Double]
    // def airmassEnd: F[Double]
    // def carouselMode: F[String]
    // def crFollow: F[Int]
    // def crTrackingFrame: F[String]
    // def parallacticAngle: F[Angle]
    // def m2UserFocusOffset: F[Double]
    // def pwfs1IntegrationTime: F[Double]
    // def pwfs2IntegrationTime: F[Double]
    // // Attribute must be changed back to Double after EPICS channel is fixed.
    // def oiwfsIntegrationTime: F[Double]
    // def aoGuideStarX: F[Double]
    // def aoGuideStarY: F[Double]
    // def aoPreparedCMX: F[Double]
    // def aoPreparedCMY: F[Double]
    // def gwfs1Target: Target[F]
    // def gwfs2Target: Target[F]
    // def gwfs3Target: Target[F]
    // def gwfs4Target: Target[F]
    // def gemsTarget(g: VirtualGemsTelescope): Target[F] = g match {
    //   case G1 => gwfs1Target
    //   case G2 => gwfs2Target
    //   case G3 => gwfs3Target
    //   case G4 => gwfs4Target
    // }
    // // GeMS statuses
    // def cwfs1Follow: F[Boolean]
    // def cwfs2Follow: F[Boolean]
    // def cwfs3Follow: F[Boolean]
    // def odgw1Follow: F[Boolean]
    // def odgw2Follow: F[Boolean]
    // def odgw3Follow: F[Boolean]
    // def odgw4Follow: F[Boolean]
    // def odgw1Parked: F[Boolean]
    // def odgw2Parked: F[Boolean]
    // def odgw3Parked: F[Boolean]
    // def odgw4Parked: F[Boolean]
    // def g1MapName: F[Option[GemsSource]]
    // def g2MapName: F[Option[GemsSource]]
    // def g3MapName: F[Option[GemsSource]]
    // def g4MapName: F[Option[GemsSource]]
    // def g1Wavelength: F[Double]
    // def g2Wavelength: F[Double]
    // def g3Wavelength: F[Double]
    // def g4Wavelength: F[Double]
    // def gemsWavelength(g: VirtualGemsTelescope): F[Double] = g match {
    //   case G1 => g1Wavelength
    //   case G2 => g2Wavelength
    //   case G3 => g3Wavelength
    //   case G4 => g4Wavelength
    // }
    // val g1GuideConfig: ProbeGuideConfig[F]
    // val g2GuideConfig: ProbeGuideConfig[F]
    // val g3GuideConfig: ProbeGuideConfig[F]
    // val g4GuideConfig: ProbeGuideConfig[F]
    // def gemsGuideConfig(g: VirtualGemsTelescope): ProbeGuideConfig[F] = g match {
    //   case G1 => g1GuideConfig
    //   case G2 => g2GuideConfig
    //   case G3 => g3GuideConfig
    //   case G4 => g4GuideConfig
    // }
  }

  object TcsStatus {

    def build[F[_]: {Async, Dispatcher}](channels: TcsChannels[F]): TcsStatus[F] =
      new TcsStatus[F] {
        override def absorbTipTilt: VerifiedEpics[F, F, Int]        =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.absorbTipTilt)
        override def m1GuideSource: VerifiedEpics[F, F, String]     =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m1Source)
        override def m1Guide: VerifiedEpics[F, F, BinaryOnOff]      =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m1State)
        override def m2p1Guide: VerifiedEpics[F, F, String]         =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m2P1Guide)
        override def m2p2Guide: VerifiedEpics[F, F, String]         =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m2P2Guide)
        override def m2oiGuide: VerifiedEpics[F, F, String]         =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m2OiGuide)
        override def m2aoGuide: VerifiedEpics[F, F, String]         =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m2AoGuide)
        override def comaCorrect: VerifiedEpics[F, F, BinaryOnOff]  =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m2ComaCorrection)
        override def m2GuideState: VerifiedEpics[F, F, BinaryOnOff] =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.m2State)
        override def pwfs1On: VerifiedEpics[F, F, BinaryYesNo]      =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.pwfs1Integrating)
        override def pwfs2On: VerifiedEpics[F, F, BinaryYesNo]      =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.pwfs2Integrating)
        override def oiwfsOn: VerifiedEpics[F, F, BinaryYesNo]      =
          VerifiedEpics.readChannel(channels.telltale, channels.guide.oiwfsIntegrating)
        override def nodState: VerifiedEpics[F, F, NodState]        =
          VerifiedEpics
            .readChannel(channels.telltale, channels.nodState)
            .map(_.map(Enumerated[NodState].fromTag(_).getOrElse(NodState.A)))
        override val pwfs1ProbeGuideState: ProbeGuideState[F]       =
          buildProbeGuideState(channels.telltale, channels.p1ProbeTrackingState)
        override val pwfs2ProbeGuideState: ProbeGuideState[F]       =
          buildProbeGuideState(channels.telltale, channels.p2ProbeTrackingState)
        override val oiwfsProbeGuideState: ProbeGuideState[F]       =
          buildProbeGuideState(channels.telltale, channels.oiProbeTrackingState)

        private val tcsSettleTime = FiniteDuration(2800, MILLISECONDS)
        override def waitInPosition(
          stabilizationTime: FiniteDuration,
          timeout:           FiniteDuration
        ): VerifiedEpics[F, F, Unit] = {
          val presV: VerifiedEpics[F, Resource[F, *], (Stream[F, StreamEvent[String]], F[String])] =
            for {
              valStr <- VerifiedEpics.eventStream(channels.telltale, channels.inPosition)
              valRdr <- VerifiedEpics
                          .readChannel(channels.telltale, channels.inPosition)
                          .map(Resource.pure[F, F[String]])
            } yield for {
              x <- valStr
              y <- valRdr
            } yield (x, y)

          presV.map(_.use { pair =>
            val (stream, read): (Stream[F, StreamEvent[String]], F[String]) = pair
            Temporal[F].realTime.flatMap { t0 =>
              stream
                .flatMap {
                  case StreamEvent.ValueChanged(v) => Stream(v)
                  case StreamEvent.Disconnected    =>
                    Stream.raiseError[F](new Throwable(s"TCS in-position channel disconnected"))
                  case _                           => Stream.empty
                }
                .keepAlive[F, String](stabilizationTime, read)
                .zip(Stream.eval(Temporal[F].realTime.map(_ - t0)).repeat)
                .dropWhile { case (_, t) => t < tcsSettleTime }
                .mapAccumulate(none[(String, FiniteDuration)]) { case (acc, (v, t)) =>
                  acc
                    .map { case (vOld, tOld) =>
                      if (v === vOld)
                        if (t - tOld >= stabilizationTime) (acc, v.some)
                        else (acc, none)
                      else ((v, t).some, none)
                    }
                    .getOrElse(((v, t).some, none))
                }
                .map(_._2)
                .unNone
                .filter(_ === "TRUE")
                .head
                .timeout(timeout)
                .compile
                .drain
            }
          })
        }

        //  WFS monitoring period
        val WfsMonitorDelay: FiniteDuration = FiniteDuration(500, TimeUnit.MILLISECONDS)
        val WfsSettleTime: FiniteDuration   = FiniteDuration(500, TimeUnit.MILLISECONDS)

        def waitWfsSky(integratingCh: Channel[F, BinaryYesNo], name: String)(
          timeout: FiniteDuration
        ): VerifiedEpics[F, F, Unit] = {
          val presV: VerifiedEpics[F,
                                   Resource[F, *],
                                   (Stream[F, StreamEvent[BinaryYesNo]], F[BinaryYesNo])
          ] =
            for {
              valStr <-
                VerifiedEpics.eventStream(channels.telltale, integratingCh)
              valRdr <- VerifiedEpics
                          .readChannel(channels.telltale, integratingCh)
                          .map(Resource.pure[F, F[BinaryYesNo]])
            } yield for {
              x <- valStr
              y <- valRdr
            } yield (x, y)

          presV.map(_.use { pair =>
            val (stream, read): (Stream[F, StreamEvent[BinaryYesNo]], F[BinaryYesNo]) = pair
            Temporal[F].realTime.flatMap { t0 =>
              stream
                .flatMap {
                  case StreamEvent.ValueChanged(v) => Stream(v)
                  case StreamEvent.Disconnected    =>
                    Stream.raiseError[F](
                      new Throwable(s"$name integration status channel disconnected")
                    )
                  case _                           => Stream.empty
                }
                .keepAlive[F, BinaryYesNo](WfsMonitorDelay, read)
                .zip(Stream.eval(Temporal[F].realTime.map(_ - t0)).repeat)
                .dropWhile { case (_, t) => t < WfsSettleTime }
                .mapAccumulate(none[(BinaryYesNo, FiniteDuration)]) { case (acc, (v, t)) =>
                  acc
                    .map { case (vOld, tOld) =>
                      if (v === vOld)
                        if (t - tOld >= WfsSettleTime) (acc, v.some)
                        else (acc, none)
                      else ((v, t).some, none)
                    }
                    .getOrElse(((v, t).some, none))
                }
                .map(_._2)
                .unNone
                .filter(_ === BinaryYesNo.No)
                .head
                .timeout(timeout)
                .compile
                .drain
            }
          })
        }

        override def waitPwfs1Sky(timeout: FiniteDuration): VerifiedEpics[F, F, Unit] =
          waitWfsSky(channels.guide.pwfs1Integrating, "PWFS1")(timeout)

        override def waitPwfs2Sky(timeout: FiniteDuration): VerifiedEpics[F, F, Unit] =
          waitWfsSky(channels.guide.pwfs2Integrating, "PWFS2")(timeout)

        override def waitOiwfsSky(timeout: FiniteDuration): VerifiedEpics[F, F, Unit] =
          waitWfsSky(channels.guide.oiwfsIntegrating, "OIWFS")(timeout)

        private def readTarget(
          name: String,
          ch:   Channel[F, Array[Double]]
        ): VerifiedEpics[F, F, TargetData] =
          VerifiedEpics
            .readChannel(channels.telltale, ch)
            .map(_.flatMap { v =>
              if (v.length >= 8)
                Declination
                  .fromRadians(v(1))
                  .map { dec =>
                    TargetData(
                      Coordinates(
                        RightAscension.fromRadians(v(0)),
                        dec
                      ),
                      FocalPlaneOffset(
                        FocalPlaneOffset.DeltaX(Angle.fromDoubleRadians(v(2) * Math.cos(v(1)))),
                        FocalPlaneOffset.DeltaY(Angle.fromDoubleRadians(v(3)))
                      ),
                      FocalPlaneOffset(
                        FocalPlaneOffset
                          .DeltaX(Distance.fromBigDecimalMillimeter(v(4)).toAngleInFocalPlane),
                        FocalPlaneOffset
                          .DeltaY(Distance.fromBigDecimalMillimeter(v(5)).toAngleInFocalPlane)
                      ),
                      FocalPlaneOffset(
                        FocalPlaneOffset
                          .DeltaX(Distance.fromBigDecimalMillimeter(v(6)).toAngleInFocalPlane),
                        FocalPlaneOffset
                          .DeltaY(Distance.fromBigDecimalMillimeter(v(7)).toAngleInFocalPlane)
                      )
                    ).pure[F]
                  }
                  .getOrElse(
                    Async[F].raiseError[TargetData](
                      new Error(s"Bad declination value in $name target array: $v")
                    )
                  )
              else
                Async[F]
                  .raiseError[TargetData](new Error(s"Not enough values in $name target array: $v"))
            })

        override def sourceATargetReadout: VerifiedEpics[F, F, TargetData] =
          readTarget("SourceA", channels.sourceATargetReadout)

        override def pwfs1TargetReadout: VerifiedEpics[F, F, TargetData] =
          readTarget("PWFS1", channels.pwfs1TargetReadout)

        override def pwfs2TargetReadout: VerifiedEpics[F, F, TargetData] =
          readTarget("PWFS2", channels.pwfs2TargetReadout)

        override def oiwfsTargetReadout: VerifiedEpics[F, F, TargetData] =
          readTarget("OIWFS", channels.oiwfsTargetReadout)

        override val pointingCorrectionState: PointingCorrectionState[F] =
          PointingCorrectionState.build(channels)

        override def probeGuideEnabled: VerifiedEpics[F, F, BinaryOnOff] =
          readChannel(channels.telltale, channels.guide.probeGuideEnabled)
            .map(_.map(v => BinaryOnOff.valueOf(v)).attempt.map(_.getOrElse(BinaryOnOff.Off)))

        override def probeGuideSource: VerifiedEpics[F, F, String] =
          readChannel(channels.telltale, channels.guide.probeGuideSource)

        override def probeGuideSink: VerifiedEpics[F, F, String] =
          readChannel(channels.telltale, channels.guide.probeGuideSink)
      }
  }

  val className: String = getClass.getName

  private[tcs] def buildSystem[F[_]: {Parallel, Async, Dispatcher}](
    applyCmd: GeminiApplyCommand[F],
    p1ObsCmd: ObserveCommand[F],
    p2ObsCmd: ObserveCommand[F],
    oiObsCmd: ObserveCommand[F],
    channels: TcsChannels[F]
  ): TcsEpicsSystem[F] =
    new TcsEpicsSystemImpl[F](channels,
                              new TcsEpicsImpl[F](applyCmd, channels),
                              p1ObsCmd,
                              p2ObsCmd,
                              oiObsCmd,
                              TcsStatus.build(channels)
    )

  def build[F[_]: {Dispatcher, Parallel, Async}](
    service: EpicsService[F],
    tops:    Map[String, String]
  ): Resource[F, TcsEpicsSystem[F]] = {
    def readTop(key: String): NonEmptyString =
      tops
        .get(key)
        .flatMap(NonEmptyString.from(_).toOption)
        .getOrElse(NonEmptyString.unsafeFrom(s"${key}:"))

    val top = readTop("tcs")
    val m1  = readTop("m1")

    for {
      channels <- TcsChannels.buildChannels(
                    service,
                    TcsTop(top),
                    M1Top(m1)
                  )
      applyCmd <-
        GeminiApplyCommand.build(service, channels.telltale, s"${top}apply", s"${top}applyC")
      p1ObsCmd <- ObserveCommand.build(service,
                                       channels.telltale,
                                       s"${top}apply",
                                       s"${top}applyC",
                                       channels.guide.pwfs1Integrating
                  )
      p2ObsCmd <- ObserveCommand.build(service,
                                       channels.telltale,
                                       s"${top}apply",
                                       s"${top}applyC",
                                       channels.guide.pwfs2Integrating
                  )
      oiObsCmd <- ObserveCommand.build(service,
                                       channels.telltale,
                                       s"${top}apply",
                                       s"${top}applyC",
                                       channels.guide.oiwfsIntegrating
                  )
    } yield buildSystem(applyCmd, p1ObsCmd, p2ObsCmd, oiObsCmd, channels)
  }

  case class TcsCommandsImpl[F[_]: {Monad, Parallel}](
    channels: TcsChannels[F],
    tcsEpics: TcsEpics[F],
    timeout:  FiniteDuration,
    params:   ParameterList[F]
  ) extends TcsCommands[F] {

    private def addParam(p: VerifiedEpics[F, F, Unit]): TcsCommands[F] =
      this.copy(params = params :+ p)

    private def addMultipleParams(c: ParameterList[F]): TcsCommands[F] =
      this.copy(params = params ++ c)

    override def post: VerifiedEpics[F, F, ApplyCommandResult] =
      tcsEpics.clear *> params.compile *> tcsEpics.post(timeout)

    override val mcsParkCommand: BaseCommand[F, TcsCommands[F]] =
      new BaseCommand[F, TcsCommands[F]] {
        override def mark: TcsCommands[F] = addParam(tcsEpics.mountParkCmd.mark)
      }

    override val mcsFollowCommand: FollowCommand[F, TcsCommands[F]] =
      (enable: Boolean) =>
        addParam(
          tcsEpics.mountFollowCmd.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

    override val rotStopCommand: RotStopCommand[F, TcsCommands[F]] =
      (enable: Boolean) =>
        addParam(
          tcsEpics.rotStopCmd.setParam1(enable.fold(BinaryYesNo.Yes, BinaryYesNo.No))
        )

    override val rotParkCommand: BaseCommand[F, TcsCommands[F]] =
      new BaseCommand[F, TcsCommands[F]] {
        override def mark: TcsCommands[F] = addParam(tcsEpics.rotParkCmd.mark)
      }

    override val rotFollowCommand: FollowCommand[F, TcsCommands[F]] =
      (enable: Boolean) =>
        addParam(
          tcsEpics.rotFollowCmd.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

    override val rotMoveCommand: RotMoveCommand[F, TcsCommands[F]] =
      (angle: RotatorAngle) =>
        addParam(
          tcsEpics.rotMoveCmd.setParam1(angle.toDoubleDegrees)
        )

    override val ecsCarouselModeCmd: CarouselModeCommand[F, TcsCommands[F]] =
      new CarouselModeCommand[F, TcsCommands[F]] {
        override def setDomeMode(mode: DomeMode): TcsCommands[F] = addParam(
          tcsEpics.carouselModeCmd.setParam1(mode.tag)
        )

        override def setShutterMode(mode: ShutterMode): TcsCommands[F] = addParam(
          tcsEpics.carouselModeCmd.setParam2(mode.tag)
        )

        override def setSlitHeight(height: Double): TcsCommands[F] = addParam(
          tcsEpics.carouselModeCmd.setParam3(height)
        )

        override def setDomeEnable(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.carouselModeCmd.setParam4(
            enable.fold[BinaryOnOff](BinaryOnOff.On, BinaryOnOff.Off)
          )
        )

        override def setShutterEnable(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.carouselModeCmd.setParam5(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )
      }

    override val ecsCarouselMoveCmd: CarouselMoveCommand[F, TcsCommands[F]] =
      (angle: Angle) =>
        addParam(
          tcsEpics.carouselMoveCmd.setParam1(angle.toDoubleDegrees)
        )

    override val ecsShuttersMoveCmd: ShuttersMoveCommand[F, TcsCommands[F]] =
      new ShuttersMoveCommand[F, TcsCommands[F]] {
        override def setTop(pos: Double): TcsCommands[F] = addParam(
          tcsEpics.shuttersMoveCmd.setParam1(pos)
        )

        override def setBottom(pos: Double): TcsCommands[F] = addParam(
          tcsEpics.shuttersMoveCmd.setParam2(pos)
        )
      }

    override val ecsVenGatesMoveCmd: VentGatesMoveCommand[F, TcsCommands[F]] =
      new VentGatesMoveCommand[F, TcsCommands[F]] {
        override def setVentGateEast(pos: Double): TcsCommands[F] = addParam(
          tcsEpics.ventGatesMoveCmd.setParam1(pos)
        )

        override def setVentGateWest(pos: Double): TcsCommands[F] = addParam(
          tcsEpics.ventGatesMoveCmd.setParam2(pos)
        )
      }

    private def buildTargetCommand(
      targetChannels: TargetChannels[F]
    ): TargetCommand[F, TcsCommands[F]] =
      new TargetCommand[F, TcsCommands[F]] {
        override def objectName(v: String): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.objectName)(v)
        )

        override def coordSystem(v: String): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.coordSystem)(v)
        )

        override def coord1(v: String): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.coord1)(v)
        )

        override def coord2(v: String): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.coord2)(v)
        )

        override def epoch(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.epoch)(v)
        )

        override def equinox(v: String): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.equinox)(v)
        )

        override def parallax(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.parallax)(v)
        )

        override def properMotion1(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.properMotion1)(v)
        )

        override def properMotion2(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.properMotion2)(v)
        )

        override def radialVelocity(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.radialVelocity)(v)
        )

        override def brightness(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.brightness)(v)
        )

        override def ephemerisFile(v: String): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, targetChannels.ephemerisFile)(v)
        )
      }

    override val sourceACmd: TargetCommand[F, TcsCommands[F]] = buildTargetCommand(channels.sourceA)

    override val pwfs1TargetCmd: TargetCommand[F, TcsCommands[F]] = buildTargetCommand(
      channels.pwfs1Target
    )

    override val pwfs2TargetCmd: TargetCommand[F, TcsCommands[F]] = buildTargetCommand(
      channels.pwfs2Target
    )

    override val oiwfsTargetCmd: TargetCommand[F, TcsCommands[F]] = buildTargetCommand(
      channels.oiwfsTarget
    )

    override val sourceAWavel: WavelengthCommand[F, TcsCommands[F]] = { (v: Wavelength) =>
      addParam(
        writeCadParam(channels.telltale, channels.wavelSourceA)(
          Wavelength.decimalMicrometers.reverseGet(v).doubleValue
        )
      )
    }

    override val pwfs1Wavel: WavelengthCommand[F, TcsCommands[F]] = { (v: Wavelength) =>
      addParam(
        writeCadParam(channels.telltale, channels.wavelPwfs1)(
          Wavelength.decimalMicrometers.reverseGet(v).doubleValue
        )
      )
    }

    override val pwfs2Wavel: WavelengthCommand[F, TcsCommands[F]] = { (v: Wavelength) =>
      addParam(
        writeCadParam(channels.telltale, channels.wavelPwfs2)(
          Wavelength.decimalMicrometers.reverseGet(v).doubleValue
        )
      )
    }

    override val oiwfsWavel: WavelengthCommand[F, TcsCommands[F]] = { (v: Wavelength) =>
      addParam(
        writeCadParam(channels.telltale, channels.wavelOiwfs)(
          Wavelength.decimalMicrometers.reverseGet(v).doubleValue
        )
      )
    }

    override val slewOptionsCommand: SlewOptionsCommand[F, TcsCommands[F]] =
      new SlewOptionsCommand[F, TcsCommands[F]] {
        override def zeroChopThrow(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroChopThrow(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def zeroSourceOffset(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroSourceOffset(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def zeroSourceDiffTrack(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroSourceDiffTrack(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def zeroMountOffset(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroMountOffset(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def zeroMountDiffTrack(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroMountDiffTrack(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def shortcircuitTargetFilter(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.shortcircuitTargetFilter(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def shortcircuitMountFilter(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.shortcircuitMountFilter(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def resetPointing(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.resetPointing(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def stopGuide(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.stopGuide(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def zeroGuideOffset(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroGuideOffset(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def zeroInstrumentOffset(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.zeroInstrumentOffset(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def autoparkPwfs1(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.autoparkPwfs1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def autoparkPwfs2(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.autoparkPwfs2(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def autoparkOiwfs(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.autoparkOiwfs(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def autoparkGems(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.autoparkGems(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def autoparkAowfs(enable: Boolean): TcsCommands[F] = addParam(
          tcsEpics.slewCmd.autoparkAowfs(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )
      }
    override val rotatorCommand: RotatorCommand[F, TcsCommands[F]]         =
      new RotatorCommand[F, TcsCommands[F]] {
        override def ipa(v: Angle): TcsCommands[F] = addParam(
          tcsEpics.rotatorConfigCmd.setParam1(v.toDoubleDegrees)
        )

        override def system(v: String): TcsCommands[F] = addParam(
          tcsEpics.rotatorConfigCmd.setParam2(v)
        )

        override def equinox(v: String): TcsCommands[F] = addParam(
          tcsEpics.rotatorConfigCmd.setParam3(v)
        )

        override def iaa(v: Angle): TcsCommands[F] = addParam(
          tcsEpics.rotatorConfigCmd.setParam4(v.toDoubleDegrees)
        )
      }

    override val originCommand: OriginCommand[F, TcsCommands[F]] =
      new OriginCommand[F, TcsCommands[F]] {
        override def originX(v: Angle): TcsCommands[F] =
          addMultipleParams(
            List(
              tcsEpics.originCmd.setParam1(v.toLengthInFocalPlane.toMillimeters.value.toDouble),
              tcsEpics.originCmd.setParam3(v.toLengthInFocalPlane.toMillimeters.value.toDouble),
              tcsEpics.originCmd.setParam5(v.toLengthInFocalPlane.toMillimeters.value.toDouble)
            )
          )

        override def originY(v: Angle): TcsCommands[F] =
          addMultipleParams(
            List(
              tcsEpics.originCmd.setParam2(v.toLengthInFocalPlane.toMillimeters.value.toDouble),
              tcsEpics.originCmd.setParam4(v.toLengthInFocalPlane.toMillimeters.value.toDouble),
              tcsEpics.originCmd.setParam6(v.toLengthInFocalPlane.toMillimeters.value.toDouble)
            )
          )
      }

    override val focusOffsetCommand: FocusOffsetCommand[F, TcsCommands[F]] =
      new FocusOffsetCommand[F, TcsCommands[F]] {
        override def focusOffset(v: Distance): TcsCommands[F] = addParam(
          // En que formato recibe Epics este valor Milimetros, Metros, Micrometros????
          tcsEpics.focusOffsetCmd.setParam1(v.toMillimeters.value.toDouble)
        )
      }

    private def buildProbeTrackingCommand(
      probeChannels: ProbeTrackingChannels[F]
    ): ProbeTrackingCommand[F, TcsCommands[F]] =
      new ProbeTrackingCommand[F, TcsCommands[F]] {
        override def nodAchopA(v: Boolean): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, probeChannels.nodachopa)(
            v.fold(BinaryOnOff.On, BinaryOnOff.Off)
          )
        )

        override def nodAchopB(v: Boolean): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, probeChannels.nodachopb)(
            v.fold(BinaryOnOff.On, BinaryOnOff.Off)
          )
        )

        override def nodBchopA(v: Boolean): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, probeChannels.nodbchopa)(
            v.fold(BinaryOnOff.On, BinaryOnOff.Off)
          )
        )

        override def nodBchopB(v: Boolean): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, probeChannels.nodbchopb)(
            v.fold(BinaryOnOff.On, BinaryOnOff.Off)
          )
        )
      }

    private def buildProbeCommands(
      probeChannels: ProbeChannels[F]
    ): ProbeCommands[F, TcsCommands[F]] =
      new ProbeCommands[F, TcsCommands[F]] {
        override val park: BaseCommand[F, TcsCommands[F]]     = new BaseCommand[F, TcsCommands[F]] {
          override def mark: TcsCommands[F] = addParam(
            ParameterlessCommandChannels[F](channels.telltale, probeChannels.parkDir).mark
          )
        }
        override val follow: FollowCommand[F, TcsCommands[F]] = (enable: Boolean) =>
          addParam(
            writeCadParam(channels.telltale, probeChannels.follow)(
              enable.fold(BinaryOnOff.On, BinaryOnOff.Off)
            )
          )
      }

    override val pwfs1ProbeTrackingCommand: ProbeTrackingCommand[F, TcsCommands[F]] =
      buildProbeTrackingCommand(channels.p1ProbeTracking)

    override val pwfs1ProbeCommands: ProbeCommands[F, TcsCommands[F]] = buildProbeCommands(
      channels.p1Probe
    )

    override val pwfs2ProbeTrackingCommand: ProbeTrackingCommand[F, TcsCommands[F]] =
      buildProbeTrackingCommand(channels.p2ProbeTracking)

    override val pwfs2ProbeCommands: ProbeCommands[F, TcsCommands[F]] = buildProbeCommands(
      channels.p2Probe
    )

    override val oiwfsProbeTrackingCommand: ProbeTrackingCommand[F, TcsCommands[F]] =
      buildProbeTrackingCommand(channels.oiProbeTracking)

    override val oiwfsProbeCommands: ProbeCommands[F, TcsCommands[F]] = buildProbeCommands(
      channels.oiProbe
    )

    override val m1GuideCommand: GuideCommand[F, TcsCommands[F]] =
      (enable: Boolean) =>
        addParam(
          tcsEpics.m1GuideCmd.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

    override val m1GuideConfigCommand: M1GuideConfigCommand[F, TcsCommands[F]] =
      new M1GuideConfigCommand[F, TcsCommands[F]] {
        override def weighting(v: String): TcsCommands[F] = addParam(
          tcsEpics.m1GuideConfigCmd.setParam1(v)
        )

        override def source(v: String): TcsCommands[F] = addParam(
          tcsEpics.m1GuideConfigCmd.setParam2(v)
        )

        override def frames(v: Int): TcsCommands[F] = addParam(
          tcsEpics.m1GuideConfigCmd.setParam3(v)
        )

        override def filename(v: String): TcsCommands[F] = addParam(
          tcsEpics.m1GuideConfigCmd.setParam4(v)
        )
      }

    override val m2GuideCommand: GuideCommand[F, TcsCommands[F]] =
      (enable: Boolean) =>
        addParam(
          tcsEpics.m2GuideCmd.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

    override val m2GuideModeCommand: M2GuideModeCommand[F, TcsCommands[F]] =
      (enable: Boolean) =>
        addParam(
          tcsEpics.m2GuideModeCmd.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

    override val m2GuideConfigCommand: M2GuideConfigCommand[F, TcsCommands[F]] =
      new M2GuideConfigCommand[F, TcsCommands[F]] {
        override def source(v: String): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam1(v)
        )

        override def sampleFreq(v: Double): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam2(v)
        )

        override def filter(v: String): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam3(v)
        )

        override def freq1(v: Option[Double]): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam4(v)
        )

        override def freq2(v: Option[Double]): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam5(v)
        )

        override def beam(v: String): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam6(v)
        )

        override def reset(v: Boolean): TcsCommands[F] = addParam(
          tcsEpics.m2GuideConfigCmd.setParam7(v.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )
      }

    override val m2GuideResetCommand: BaseCommand[F, TcsCommands[F]] =
      new BaseCommand[F, TcsCommands[F]] {
        override def mark: TcsCommands[F] = addParam(tcsEpics.m2GuideResetCmd.mark)
      }

    override val mountGuideCommand: MountGuideCommand[F, TcsCommands[F]] =
      new MountGuideCommand[F, TcsCommands[F]] {
        override def mode(v: Boolean): TcsCommands[F] = addParam(
          tcsEpics.mountGuideCmd.setParam1(v.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

        override def source(v: String): TcsCommands[F] = addParam(
          tcsEpics.mountGuideCmd.setParam2(v)
        )

        override def p1Weight(v: Double): TcsCommands[F] = addParam(
          tcsEpics.mountGuideCmd.setParam3(v)
        )

        override def p2Weight(v: Double): TcsCommands[F] = addParam(
          tcsEpics.mountGuideCmd.setParam4(v)
        )
      }

    private def buildWfsProcCommands(
      wfsChannels: WfsChannels[F]
    ): WfsProcCommands[F, TcsCommands[F]] =
      new WfsProcCommands[F, TcsCommands[F]] {
        override val signalProc: WfsSignalProcConfigCommand[F, TcsCommands[F]] = (v: String) =>
          addParam(
            writeCadParam(channels.telltale, wfsChannels.procParams)(v)
          )
        override val dark: WfsDarkCommand[F, TcsCommands[F]]                   =
          new WfsDarkCommand[F, TcsCommands[F]] {
            override def filename(v: String): TcsCommands[F] = addParam(
              writeCadParam(channels.telltale, wfsChannels.dark)(v)
            )
          }
        override val closedLoop: WfsClosedLoopCommand[F, TcsCommands[F]]       =
          new WfsClosedLoopCommand[F, TcsCommands[F]] {
            override def global(v: Double): TcsCommands[F] = addParam(
              writeCadParam(channels.telltale, wfsChannels.closedLoop.global)(v)
            )

            override def average(v: Int): TcsCommands[F] = addParam(
              writeCadParam(channels.telltale, wfsChannels.closedLoop.average)(v)
            )

            override def zernikes2m2(v: Int): TcsCommands[F] = addParam(
              writeCadParam(channels.telltale, wfsChannels.closedLoop.zernikes2m2)(v)
            )

            override def mult(v: Double): TcsCommands[F] = addParam(
              writeCadParam(channels.telltale, wfsChannels.closedLoop.mult)(v)
            )
          }
      }

    override val pwfs1Commands: WfsProcCommands[F, TcsCommands[F]] = buildWfsProcCommands(
      channels.pwfs1
    )

    override val pwfs2Commands: WfsProcCommands[F, TcsCommands[F]] = buildWfsProcCommands(
      channels.pwfs2
    )

    override val probeGuideModeCommand: ProbeGuideModeCommand[F, TcsCommands[F]] =
      new ProbeGuideModeCommand[F, TcsCommands[F]] {
        override def setMode(pg: Option[ProbeGuide]): TcsCommands[F] =
          pg.fold(
            addParam(tcsEpics.probeGuideModeCmd.setParam1(BinaryOnOff.Off))
          )(pg =>
            addMultipleParams(
              List(tcsEpics.probeGuideModeCmd.setParam1(BinaryOnOff.On),
                   tcsEpics.probeGuideModeCmd.setParam2(pg.from),
                   tcsEpics.probeGuideModeCmd.setParam3(pg.to)
              )
            )
          )
      }

    override val oiwfsSelectCommand: OiwfsSelectCommand[F, TcsCommands[F]] =
      new OiwfsSelectCommand[F, TcsCommands[F]] {
        override def oiwfsName(v: String): TcsCommands[F] = addParam(
          tcsEpics.oiwfsSelectCmd.setParam1(v)
        )
        override def output(v: String): TcsCommands[F]    = addParam(
          tcsEpics.oiwfsSelectCmd.setParam2(v)
        )
      }

    override val bafflesCommand: BafflesCommand[F, TcsCommands[F]]                            =
      new BafflesCommand[F, TcsCommands[F]] {

        override def central(v: CentralBafflePosition): TcsCommands[F] = addParam(
          tcsEpics.bafflesCmd.setParam1(v)
        )

        override def deployable(v: DeployableBafflePosition): TcsCommands[F] = addParam(
          tcsEpics.bafflesCmd.setParam2(v)
        )
      }
    override val m2FollowCommand: FollowCommand[F, TcsCommands[F]]                            =
      (enable: Boolean) =>
        addParam(
          tcsEpics.m2FollowCmd.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
        )

    private def buildAgMechCommands[A](
      c: AgMechCommandsChannels[F, A]
    ): AgMechCommands[F, A, TcsCommands[F]] =
      new AgMechCommands[F, A, TcsCommands[F]] {
        override val park: BaseCommand[F, TcsCommands[F]]    = new BaseCommand[F, TcsCommands[F]] {
          override def mark: TcsCommands[F] = addParam(c.park.mark)
        }
        override val move: MoveCommand[F, A, TcsCommands[F]] =
          new MoveCommand[F, A, TcsCommands[F]] {
            override def setPosition(v: A): TcsCommands[F] = addParam(c.position.setParam1(v))
          }
      }
    override val hrwfsCommands: AgMechCommands[F, HrwfsPickupPosition, TcsCommands[F]]        =
      buildAgMechCommands(
        tcsEpics.hrwfsCmds
      )
    override val scienceFoldCommands: AgMechCommands[F, ScienceFold.Position, TcsCommands[F]] =
      buildAgMechCommands(
        tcsEpics.scienceFoldCmds
      )
    override val aoFoldCommands: AgMechCommands[F, AoFoldPosition, TcsCommands[F]]            =
      buildAgMechCommands(
        tcsEpics.aoFoldCmds
      )
    override val m1Commands: M1Commands[F, TcsCommands[F]]                                    = new M1Commands[F, TcsCommands[F]] {

      override def park: TcsCommands[F] = addParam(tcsEpics.m1Cmds.park.setParam1("PARK"))

      override def unpark: TcsCommands[F] = addParam(tcsEpics.m1Cmds.park.setParam1("UNPARK"))

      override def figureUpdates(enable: Boolean): TcsCommands[F] = addParam(
        tcsEpics.m1Cmds.figureUpdates.setParam1(enable.fold(BinaryOnOff.On, BinaryOnOff.Off))
      )

      override def zero(mech: String): TcsCommands[F] = addParam(
        tcsEpics.m1Cmds.zero.setParam1(mech)
      )

      override def saveModel(name: String): TcsCommands[F] = addParam(
        tcsEpics.m1Cmds.saveModel.setParam1(name)
      )

      override def loadModel(name: String): TcsCommands[F] = addParam(
        tcsEpics.m1Cmds.loadModel.setParam1(name)
      )

      override def ao(enable: Boolean): TcsCommands[F] = addParam(
        tcsEpics.m1Cmds.ao(enable.fold(BinaryOnOffCapitalized.On, BinaryOnOffCapitalized.Off))
      )
    }

    override val targetAdjustCommand: AdjustCommand[F, TcsCommands[F]] =
      new AdjustCommand[F, TcsCommands[F]] {
        override def frame(frm: ReferenceFrame): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetAdjust.frame)(frm)
        )

        override def size(sz: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetAdjust.size)(sz)
        )

        override def angle(a: Angle): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetAdjust.angle)(a.toDoubleDegrees)
        )

        override def vtMask(vts: List[VirtualTelescope]): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetAdjust.vtMask)(vts)
        )
      }

    override val originAdjustCommand: AdjustCommand[F, TcsCommands[F]] =
      new AdjustCommand[F, TcsCommands[F]] {
        override def frame(frm: ReferenceFrame): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originAdjust.frame)(frm)
        )

        override def size(sz: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originAdjust.size)(sz)
        )

        override def angle(a: Angle): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originAdjust.angle)(a.toDoubleDegrees)
        )

        override def vtMask(vts: List[VirtualTelescope]): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originAdjust.vtMask)(vts)
        )
      }

    override val targetFilter: TargetFilterCommand[F, TcsCommands[F]]                =
      new TargetFilterCommand[F, TcsCommands[F]] {
        override def bandwidth(bw: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetFilter.bandWidth)(bw)
        )

        override def maxVelocity(mv: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetFilter.maxVelocity)(mv)
        )

        override def grabRadius(gr: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetFilter.grabRadius)(gr)
        )

        override def shortcircuit(sc: ShortcircuitTargetFilter.Type): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetFilter.shortCircuit)(
            sc.value.fold("Closed", "Open")
          )
        )
      }
    override val pointingAdjustCommand: PointingAdjustCommand[F, TcsCommands[F]]     =
      new PointingAdjustCommand[F, TcsCommands[F]] {
        override def frame(frm: ReferenceFrame): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.pointingAdjust.frame)(frm)
        )

        override def size(sz: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.pointingAdjust.size)(sz)
        )

        override def angle(a: Angle): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.pointingAdjust.angle)(a.toDoubleDegrees)
        )
      }
    override val pointingConfigCommand: PointingConfigCommand[F, TcsCommands[F]]     =
      new PointingConfigCommand[F, TcsCommands[F]] {
        override def name(nm: PointingParameter): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.pointingConfig.name)(nm)
        )

        override def level(lv: PointingConfigLevel): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.pointingConfig.level)(lv)
        )

        override def value(v: Double): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.pointingConfig.value)(v)
        )
      }
    override val targetOffsetAbsorb: OffsetMgmCommand[F, TcsCommands[F]]             =
      new OffsetMgmCommand[F, TcsCommands[F]] {

        override def vt(v: VirtualTelescope): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetOffsetAbsorb.vt)(v)
        )

        override def index(v: OffsetIndexSelection): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetOffsetAbsorb.index)(v)
        )
      }
    override val targetOffsetClear: OffsetMgmCommand[F, TcsCommands[F]]              =
      new OffsetMgmCommand[F, TcsCommands[F]] {

        override def vt(v: VirtualTelescope): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetOffsetClear.vt)(v)
        )

        override def index(v: OffsetIndexSelection): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.targetOffsetClear.index)(v)
        )
      }
    override val originOffsetAbsorb: OffsetMgmCommand[F, TcsCommands[F]]             =
      new OffsetMgmCommand[F, TcsCommands[F]] {

        override def vt(v: VirtualTelescope): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originOffsetAbsorb.vt)(v)
        )

        override def index(v: OffsetIndexSelection): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originOffsetAbsorb.index)(v)
        )
      }
    override val originOffsetClear: OffsetMgmCommand[F, TcsCommands[F]]              =
      new OffsetMgmCommand[F, TcsCommands[F]] {

        override def vt(v: VirtualTelescope): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originOffsetClear.vt)(v)
        )

        override def index(v: OffsetIndexSelection): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.originOffsetClear.index)(v)
        )
      }
    override val absorbGuideCommand: BaseCommand[F, TcsCommands[F]]                  =
      new BaseCommand[F, TcsCommands[F]] {
        override def mark: TcsCommands[F] = addParam(
          writeChannel(channels.telltale, channels.absorbGuideDir)(CadDirective.MARK.pure[F])
        )
      }
    override val zeroGuideCommand: BaseCommand[F, TcsCommands[F]]                    =
      new BaseCommand[F, TcsCommands[F]] {
        override def mark: TcsCommands[F] = addParam(
          writeChannel(channels.telltale, channels.zeroGuideDir)(CadDirective.MARK.pure[F])
        )
      }
    override val instrumentOffsetCommand: InstrumentOffsetCommand[F, TcsCommands[F]] =
      new InstrumentOffsetCommand[F, TcsCommands[F]] {
        override def offsetX(v: Distance): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.instrumentOffset.x)(
            v.toMillimeters.value.toDouble
          )
        )

        override def offsetY(v: Distance): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.instrumentOffset.y)(
            v.toMillimeters.value.toDouble
          )
        )
      }
    override val wrapsCommand: WrapsCommand[F, TcsCommands[F]]                       =
      new WrapsCommand[F, TcsCommands[F]] {
        override def azimuth(v: Int): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.azimuthWrap)(v)
        )

        override def rotator(v: Int): TcsCommands[F] = addParam(
          writeCadParam(channels.telltale, channels.rotatorWrap)(v)
        )
      }
    override val zeroRotatorGuide: BaseCommand[F, TcsCommands[F]]                    =
      new BaseCommand[F, TcsCommands[F]] {
        override def mark: TcsCommands[F] = addParam(
          writeChannel(channels.telltale, channels.zeroRotatorGuideDir)(CadDirective.MARK.pure[F])
        )
      }

    private def buildPwfsMechCommands(
      chs: PwfsMechCmdChannels[F]
    ): PwfsMechCommands[F] = new PwfsMechCommands[F] {
      override def filter(f: PwfsFilter): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, chs.filter)(f)
      )

      override def fieldStop(fs: PwfsFieldStop): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, chs.fieldStop)(fs)
      )
    }

    override val pwfs1MechCommands: PwfsMechCommands[F] = buildPwfsMechCommands(channels.pwfs1Mechs)

    override val pwfs2MechCommands: PwfsMechCommands[F] = buildPwfsMechCommands(channels.pwfs2Mechs)

    override val chopConfig: ChopConfigCommand[F]     = new ChopConfigCommand[F] {
      override def typ(tp: String): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopConfig.typ)(tp)
      )

      override def source(src: ChopSource): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopConfig.source)(src)
      )

      override def frequency(f: Double): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopConfig.frequency)(f)
      )

      override def dutyCycle(duty: Double): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopConfig.dutyCycle)(duty)
      )
    }
    override val chopRelative: ChopRelativeCommand[F] = new ChopRelativeCommand[F] {
      override def thrw(d: Angle): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopRelative.thrw)(
          Angle.signedDecimalArcseconds.get(d).toDouble
        )
      )

      override def angle(a: Angle): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopRelative.angle)(a.toDoubleDegrees)
      )

      override def system(sys: String): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopRelative.system)(sys)
      )

      override def equinox(eq: String): TcsCommands[F] = addParam(
        writeCadParam(channels.telltale, channels.chopRelative.equinox)(eq)
      )
    }
  }

  trait WfsCommands[F[_]] {
    def post(typ: ObserveCommand.CommandType): VerifiedEpics[F, F, ApplyCommandResult]

    val observe: WfsObserveCommand[F, WfsCommands[F[_]]]
    val stop: BaseCommand[F, WfsCommands[F[_]]]
  }

  case class WfsCommandsImpl[F[_]: {Monad, Parallel}](
    channels:    TcsChannels[F],
    wfsChannels: WfsChannels[F],
    wfsCmd:      ObserveCommand[F],
    timeout:     FiniteDuration,
    params:      ParameterList[F]
  ) extends WfsCommands[F] {

    private def addParam(p: VerifiedEpics[F, F, Unit]): WfsCommands[F] =
      this.copy(params = params :+ p)

    override def post(typ: ObserveCommand.CommandType): VerifiedEpics[F, F, ApplyCommandResult] =
      wfsCmd.clearIfNotBusy *> params.compile *> wfsCmd.post(typ, timeout)

    override val observe: WfsObserveCommand[F, WfsCommands[F[_]]] =
      new WfsObserveCommand[F, WfsCommands[F]] {
        override def numberOfExposures(v: Int): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.numberOfExposures)(v)
        )

        override def interval(v: Double): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.interval)(v)
        )

        override def options(v: String): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.options)(v)
        )

        override def label(v: String): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.label)(v)
        )

        override def output(v: String): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.output)(v)
        )

        override def path(v: String): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.path)(v)
        )

        override def fileName(v: String): WfsCommands[F] = addParam(
          writeCadParam(channels.telltale, wfsChannels.observe.fileName)(v)
        )
      }
    override val stop: BaseCommand[F, WfsCommands[F[_]]]          = new BaseCommand[F, WfsCommands[F]] {
      override def mark: WfsCommands[F] = addParam(
        writeChannel(channels.telltale, wfsChannels.stop)(CadDirective.MARK.pure[F])
      )
    }
  }

  trait PwfsMechCommands[F[_]] {
    def filter(f:     PwfsFilter): TcsCommands[F]
    def fieldStop(fs: PwfsFieldStop): TcsCommands[F]
  }

  class TcsEpicsSystemImpl[F[_]: {Monad, Parallel}](
    channels: TcsChannels[F],
    epics:    TcsEpics[F],
    p1ObsCmd: ObserveCommand[F],
    p2ObsCmd: ObserveCommand[F],
    oiObsCmd: ObserveCommand[F],
    st:       TcsStatus[F]
  ) extends TcsEpicsSystem[F] {
    override def startCommand(timeout: FiniteDuration): TcsCommands[F] =
      TcsCommandsImpl(channels, epics, timeout, List.empty)

    override val status: TcsStatus[F] = st

    override def startPwfs1Command(timeout: FiniteDuration): WfsCommands[F] =
      WfsCommandsImpl(channels, channels.pwfs1, p1ObsCmd, timeout, List.empty)

    override def startPwfs2Command(timeout: FiniteDuration): WfsCommands[F] =
      WfsCommandsImpl(channels, channels.pwfs2, p2ObsCmd, timeout, List.empty)

    override def startOiwfsCommand(timeout: FiniteDuration): WfsCommands[F] =
      WfsCommandsImpl(channels, channels.oiwfs, oiObsCmd, timeout, List.empty)

    override def clearErrors: VerifiedEpics[F, F, Unit] = epics.clear
  }

  class TcsEpicsImpl[F[_]: Monad](
    applyCmd: GeminiApplyCommand[F],
    channels: TcsChannels[F]
  ) extends TcsEpics[F] {

    override def post(timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult] =
      applyCmd.post(timeout)

    override def clear: VerifiedEpics[F, F, Unit] = applyCmd.clearIfNotBusy

    override val mountParkCmd: ParameterlessCommandChannels[F] =
      ParameterlessCommandChannels(channels.telltale, channels.telescopeParkDir)

    override val mountFollowCmd: Command1Channels[F, BinaryOnOff] =
      Command1Channels(channels.telltale, channels.mountFollow)

    override val rotStopCmd: Command1Channels[F, BinaryYesNo] =
      Command1Channels(channels.telltale, channels.rotStopBrake)

    override val rotParkCmd: ParameterlessCommandChannels[F] =
      ParameterlessCommandChannels(channels.telltale, channels.rotParkDir)

    override val rotFollowCmd: Command1Channels[F, BinaryOnOff] =
      Command1Channels(channels.telltale, channels.rotFollow)

    override val rotMoveCmd: Command1Channels[F, Double] =
      Command1Channels(channels.telltale, channels.rotMoveAngle)

    override val carouselModeCmd
      : Command5Channels[F, String, String, Double, BinaryOnOff, BinaryOnOff] =
      Command5Channels(
        channels.telltale,
        channels.enclosure.ecsDomeMode,
        channels.enclosure.ecsShutterMode,
        channels.enclosure.ecsSlitHeight,
        channels.enclosure.ecsDomeEnable,
        channels.enclosure.ecsShutterEnable
      )

    override val carouselMoveCmd: Command1Channels[F, Double] =
      Command1Channels(channels.telltale, channels.enclosure.ecsMoveAngle)

    override val shuttersMoveCmd: Command2Channels[F, Double, Double] =
      Command2Channels(channels.telltale,
                       channels.enclosure.ecsShutterTop,
                       channels.enclosure.ecsShutterBottom
      )

    override val ventGatesMoveCmd: Command2Channels[F, Double, Double] =
      Command2Channels(channels.telltale,
                       channels.enclosure.ecsVentGateEast,
                       channels.enclosure.ecsVentGateWest
      )

    override val slewCmd: SlewCommandChannels[F] =
      SlewCommandChannels(channels.telltale, channels.slew)

    override val rotatorConfigCmd: Command4Channels[F, Double, String, String, Double] =
      Command4Channels(
        channels.telltale,
        channels.rotator.ipa,
        channels.rotator.system,
        channels.rotator.equinox,
        channels.rotator.iaa
      )

    override val originCmd: Command6Channels[F, Double, Double, Double, Double, Double, Double] =
      Command6Channels(
        channels.telltale,
        channels.origin.xa,
        channels.origin.ya,
        channels.origin.xb,
        channels.origin.yb,
        channels.origin.xc,
        channels.origin.yc
      )

    override val focusOffsetCmd: Command1Channels[F, Double] = Command1Channels(
      channels.telltale,
      channels.focusOffset
    )

    override val oiwfsProbeTrackingCmd: ProbeTrackingCommandChannels[F] =
      ProbeTrackingCommandChannels(
        channels.telltale,
        channels.oiProbeTracking
      )
    override val oiwfsProbeCmds: ProbeCommandsChannels[F]               =
      buildProbeCommandsChannels(channels.telltale, channels.oiProbe)

    override val m1GuideConfigCmd: Command4Channels[F, String, String, Int, String] =
      Command4Channels(
        channels.telltale,
        channels.m1GuideConfig.weighting,
        channels.m1GuideConfig.source,
        channels.m1GuideConfig.frames,
        channels.m1GuideConfig.filename
      )

    override val m1GuideCmd: Command1Channels[F, BinaryOnOff] =
      Command1Channels(channels.telltale, channels.m1Guide)

    override val m2GuideCmd: Command1Channels[F, BinaryOnOff] =
      Command1Channels(channels.telltale, channels.m2Guide)

    override val m2GuideModeCmd: Command1Channels[F, BinaryOnOff] =
      Command1Channels(channels.telltale, channels.m2GuideMode)

    override val m2GuideConfigCmd: Command7Channels[F,
                                                    String,
                                                    Double,
                                                    String,
                                                    Option[Double],
                                                    Option[Double],
                                                    String,
                                                    BinaryOnOff
    ] =
      Command7Channels(
        channels.telltale,
        channels.m2GuideConfig.source,
        channels.m2GuideConfig.samplefreq,
        channels.m2GuideConfig.filter,
        channels.m2GuideConfig.freq1,
        channels.m2GuideConfig.freq2,
        channels.m2GuideConfig.beam,
        channels.m2GuideConfig.reset
      )

    override val m2GuideResetCmd: ParameterlessCommandChannels[F] = ParameterlessCommandChannels(
      channels.telltale,
      channels.m2GuideReset
    )

    override val mountGuideCmd: Command4Channels[F, BinaryOnOff, String, Double, Double] =
      new Command4Channels(
        channels.telltale,
        channels.mountGuide.mode,
        channels.mountGuide.source,
        channels.mountGuide.p1weight,
        channels.mountGuide.p2weight
      )

    override val probeGuideModeCmd: Command3Channels[F, BinaryOnOff, GuideProbe, GuideProbe]      =
      Command3Channels(
        channels.telltale,
        channels.probeGuideMode.state,
        channels.probeGuideMode.from,
        channels.probeGuideMode.to
      )
    override val oiwfsSelectCmd: Command2Channels[F, String, String]                              = Command2Channels(
      channels.telltale,
      channels.oiwfsSelect.oiName,
      channels.oiwfsSelect.output
    )
    override val bafflesCmd: Command2Channels[F, CentralBafflePosition, DeployableBafflePosition] =
      Command2Channels(
        channels.telltale,
        channels.m2Baffles.centralBaffle,
        channels.m2Baffles.deployBaffle
      )
    override val m2FollowCmd: Command1Channels[F, BinaryOnOff]                                    = Command1Channels(
      channels.telltale,
      channels.m2Follow
    )
    override val hrwfsCmds: AgMechCommandsChannels[F, HrwfsPickupPosition]                        =
      buildAgMechCommandsChannels(channels.telltale, channels.hrwfsMech)
    override val scienceFoldCmds: AgMechCommandsChannels[F, ScienceFold.Position]                 =
      buildAgMechCommandsChannels(channels.telltale, channels.scienceFoldMech)
    override val aoFoldCmds: AgMechCommandsChannels[F, AoFoldPosition]                            =
      buildAgMechCommandsChannels(channels.telltale, channels.aoFoldMech)
    override val m1Cmds: M1CommandsChannels[F]                                                    =
      M1CommandsChannels.build(channels.telltale, channels.m1Channels)
  }

  case class TargetCommandChannels[F[_]: Monad](
    tt:             TelltaleChannel[F],
    targetChannels: TargetChannels[F]
  ) {
    def objectName(v: String): VerifiedEpics[F, F, Unit]     =
      writeCadParam[F, String](tt, targetChannels.objectName)(v)
    def coordSystem(v: String): VerifiedEpics[F, F, Unit]    =
      writeCadParam[F, String](tt, targetChannels.coordSystem)(v)
    def coord1(v: String): VerifiedEpics[F, F, Unit]         =
      writeCadParam[F, String](tt, targetChannels.coord1)(v)
    def coord2(v: String): VerifiedEpics[F, F, Unit]         =
      writeCadParam[F, String](tt, targetChannels.coord2)(v)
    def epoch(v: Double): VerifiedEpics[F, F, Unit]          =
      writeCadParam[F, Double](tt, targetChannels.epoch)(v)
    def equinox(v: String): VerifiedEpics[F, F, Unit]        =
      writeCadParam[F, String](tt, targetChannels.equinox)(v)
    def parallax(v: Double): VerifiedEpics[F, F, Unit]       =
      writeCadParam[F, Double](tt, targetChannels.parallax)(v)
    def properMotion1(v: Double): VerifiedEpics[F, F, Unit]  =
      writeCadParam[F, Double](tt, targetChannels.properMotion1)(v)
    def properMotion2(v: Double): VerifiedEpics[F, F, Unit]  =
      writeCadParam[F, Double](tt, targetChannels.properMotion2)(v)
    def radialVelocity(v: Double): VerifiedEpics[F, F, Unit] =
      writeCadParam[F, Double](tt, targetChannels.radialVelocity)(v)
    def brightness(v: Double): VerifiedEpics[F, F, Unit]     =
      writeCadParam[F, Double](tt, targetChannels.brightness)(v)
    def ephemerisFile(v: String): VerifiedEpics[F, F, Unit]  =
      writeCadParam[F, String](tt, targetChannels.ephemerisFile)(v)
  }

  case class SlewCommandChannels[F[_]: Monad](
    tt:           TelltaleChannel[F],
    slewChannels: SlewChannels[F]
  ) {
    def zeroChopThrow(v: BinaryOnOff): VerifiedEpics[F, F, Unit]            =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroChopThrow)(v)
    def zeroSourceOffset(v: BinaryOnOff): VerifiedEpics[F, F, Unit]         =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroSourceOffset)(v)
    def zeroSourceDiffTrack(v: BinaryOnOff): VerifiedEpics[F, F, Unit]      =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroSourceDiffTrack)(v)
    def zeroMountOffset(v: BinaryOnOff): VerifiedEpics[F, F, Unit]          =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroMountOffset)(v)
    def zeroMountDiffTrack(v: BinaryOnOff): VerifiedEpics[F, F, Unit]       =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroMountDiffTrack)(v)
    def shortcircuitTargetFilter(v: BinaryOnOff): VerifiedEpics[F, F, Unit] =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.shortcircuitTargetFilter)(v)
    def shortcircuitMountFilter(v: BinaryOnOff): VerifiedEpics[F, F, Unit]  =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.shortcircuitMountFilter)(v)
    def resetPointing(v: BinaryOnOff): VerifiedEpics[F, F, Unit]            =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.resetPointing)(v)
    def stopGuide(v: BinaryOnOff): VerifiedEpics[F, F, Unit]                =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.stopGuide)(v)
    def zeroGuideOffset(v: BinaryOnOff): VerifiedEpics[F, F, Unit]          =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroGuideOffset)(v)
    def zeroInstrumentOffset(v: BinaryOnOff): VerifiedEpics[F, F, Unit]     =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.zeroInstrumentOffset)(v)
    def autoparkPwfs1(v: BinaryOnOff): VerifiedEpics[F, F, Unit]            =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.autoparkPwfs1)(v)
    def autoparkPwfs2(v: BinaryOnOff): VerifiedEpics[F, F, Unit]            =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.autoparkPwfs2)(v)
    def autoparkOiwfs(v: BinaryOnOff): VerifiedEpics[F, F, Unit]            =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.autoparkOiwfs)(v)
    def autoparkGems(v: BinaryOnOff): VerifiedEpics[F, F, Unit]             =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.autoparkGems)(v)
    def autoparkAowfs(v: BinaryOnOff): VerifiedEpics[F, F, Unit]            =
      writeCadParam[F, BinaryOnOff](tt, slewChannels.autoparkAowfs)(v)
  }

  case class ProbeTrackingCommandChannels[F[_]: Monad](
    tt:                 TelltaleChannel[F],
    probeGuideChannels: ProbeTrackingChannels[F]
  ) {
    def nodAchopA(v: BinaryOnOff): VerifiedEpics[F, F, Unit] =
      writeCadParam[F, BinaryOnOff](tt, probeGuideChannels.nodachopa)(v)

    def nodAchopB(v: BinaryOnOff): VerifiedEpics[F, F, Unit] =
      writeCadParam[F, BinaryOnOff](tt, probeGuideChannels.nodachopb)(v)

    def nodBchopA(v: BinaryOnOff): VerifiedEpics[F, F, Unit] =
      writeCadParam[F, BinaryOnOff](tt, probeGuideChannels.nodbchopa)(v)

    def nodBchopB(v: BinaryOnOff): VerifiedEpics[F, F, Unit] =
      writeCadParam[F, BinaryOnOff](tt, probeGuideChannels.nodbchopb)(v)
  }

  case class ProbeCommandsChannels[F[_]](
    park:   ParameterlessCommandChannels[F],
    follow: Command1Channels[F, BinaryOnOff]
  )

  def buildProbeCommandsChannels[F[_]: Monad](
    tt:            TelltaleChannel[F],
    probeChannels: ProbeChannels[F]
  ): ProbeCommandsChannels[F] = ProbeCommandsChannels(
    ParameterlessCommandChannels(tt, probeChannels.parkDir),
    Command1Channels(tt, probeChannels.follow)
  )

  trait ProbeGuideConfig[F[_]] {
    def nodAchopA: VerifiedEpics[F, F, BinaryOnOff]
    def nodAchopB: VerifiedEpics[F, F, BinaryOnOff]
    def nodBchopA: VerifiedEpics[F, F, BinaryOnOff]
    def nodBchopB: VerifiedEpics[F, F, BinaryOnOff]
  }

  def buildProbeGuideConfig[F[_]: Applicative](
    tt:            TelltaleChannel[F],
    probeChannels: ProbeTrackingChannels[F]
  ): ProbeGuideConfig[F] = new ProbeGuideConfig[F] {

    override def nodAchopA: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodachopa)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))

    override def nodAchopB: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodachopb)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))

    override def nodBchopA: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodbchopa)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))

    override def nodBchopB: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodbchopb)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))
  }

  trait ProbeGuideState[F[_]] {
    def nodAchopA: VerifiedEpics[F, F, BinaryOnOff]
    def nodAchopB: VerifiedEpics[F, F, BinaryOnOff]
    def nodBchopA: VerifiedEpics[F, F, BinaryOnOff]
    def nodBchopB: VerifiedEpics[F, F, BinaryOnOff]
  }

  def buildProbeGuideState[F[_]: Applicative](
    tt:            TelltaleChannel[F],
    probeChannels: ProbeTrackingStateChannels[F]
  ): ProbeGuideState[F] = new ProbeGuideState[F] {

    override def nodAchopA: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodachopa)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))

    override def nodAchopB: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodachopb)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))

    override def nodBchopA: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodbchopa)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))

    override def nodBchopB: VerifiedEpics[F, F, BinaryOnOff] =
      VerifiedEpics
        .readChannel(tt, probeChannels.nodbchopb)
        .map(_.map(Enumerated[BinaryOnOff].fromTag(_).getOrElse(BinaryOnOff.Off)))
  }

  trait PointingCorrectionState[F[_]] {
    def localCA: VerifiedEpics[F, F, Angle]
    def localCE: VerifiedEpics[F, F, Angle]
    def guideCA: VerifiedEpics[F, F, Angle]
    def guideCE: VerifiedEpics[F, F, Angle]
  }

  object PointingCorrectionState {
    def build[F[_]: Applicative](channels: TcsChannels[F]): PointingCorrectionState[F] =
      new PointingCorrectionState[F] {

        override def localCA: VerifiedEpics[F, F, Angle] = VerifiedEpics
          .readChannel(channels.telltale, channels.pointingAdjustmentState.localCA)
          .map(_.map(Angle.fromDoubleArcseconds))

        override def localCE: VerifiedEpics[F, F, Angle] = VerifiedEpics
          .readChannel(channels.telltale, channels.pointingAdjustmentState.localCE)
          .map(_.map(Angle.fromDoubleArcseconds))

        override def guideCA: VerifiedEpics[F, F, Angle] = VerifiedEpics
          .readChannel(channels.telltale, channels.pointingAdjustmentState.guideCA)
          .map(_.map(Angle.fromDoubleArcseconds))

        override def guideCE: VerifiedEpics[F, F, Angle] = VerifiedEpics
          .readChannel(channels.telltale, channels.pointingAdjustmentState.guideCE)
          .map(_.map(Angle.fromDoubleArcseconds))
      }
  }

  case class AgMechCommandsChannels[F[_], A](
    park:     ParameterlessCommandChannels[F],
    position: Command1Channels[F, A]
  )

  def buildAgMechCommandsChannels[F[_]: Monad, A: Encoder[*, String]](
    tt:           TelltaleChannel[F],
    mechChannels: AgMechChannels[F]
  ): AgMechCommandsChannels[F, A] = AgMechCommandsChannels[F, A](
    ParameterlessCommandChannels(tt, mechChannels.parkDir),
    Command1Channels(tt, mechChannels.position)
  )

  case class M1CommandsChannels[F[_]](
    park:          Command1Channels[F, String],
    figureUpdates: Command1Channels[F, BinaryOnOff],
    zero:          Command1Channels[F, String],
    saveModel:     Command1Channels[F, String],
    loadModel:     Command1Channels[F, String],
    ao:            (en: BinaryOnOffCapitalized) => VerifiedEpics[F, F, Unit]
  )

  object M1CommandsChannels {
    def build[F[_]: Monad](
      tcsTt:      TelltaleChannel[F],
      m1Channels: TcsChannels.M1Channels[F]
    ): M1CommandsChannels[F] = M1CommandsChannels[F](
      park = Command1Channels(tcsTt, m1Channels.park),
      figureUpdates = Command1Channels(tcsTt, m1Channels.figUpdates),
      zero = Command1Channels[F, String](tcsTt, m1Channels.zero),
      saveModel = Command1Channels[F, String](tcsTt, m1Channels.saveModelFile),
      loadModel = Command1Channels[F, String](tcsTt, m1Channels.loadModelFile),
      ao = (en: BinaryOnOffCapitalized) =>
        writeChannel(m1Channels.telltale, m1Channels.aoEnable)(en.pure[F])
    )
  }

  case class WfsCommandsChannels[F[_]](
    observe:    Command7Channels[F, Int, Double, String, String, String, String, String],
    stop:       ParameterlessCommandChannels[F],
    signalProc: Command1Channels[F, String],
    dark:       Command1Channels[F, String],
    closedLoop: Command4Channels[F, Double, Int, Int, Double]
  )

  object WfsCommandsChannels {
    def build[F[_]: Monad](
      tt:          TelltaleChannel[F],
      wfsChannels: WfsChannels[F]
    ): WfsCommandsChannels[F] = WfsCommandsChannels(
      Command7Channels(
        tt,
        wfsChannels.observe.numberOfExposures,
        wfsChannels.observe.interval,
        wfsChannels.observe.options,
        wfsChannels.observe.label,
        wfsChannels.observe.output,
        wfsChannels.observe.path,
        wfsChannels.observe.fileName
      ),
      ParameterlessCommandChannels(tt, wfsChannels.stop),
      Command1Channels(tt, wfsChannels.procParams),
      Command1Channels(tt, wfsChannels.dark),
      Command4Channels(
        tt,
        wfsChannels.closedLoop.global,
        wfsChannels.closedLoop.average,
        wfsChannels.closedLoop.zernikes2m2,
        wfsChannels.closedLoop.mult
      )
    )
  }

  trait FollowCommand[F[_], +S] {
    def setFollow(enable: Boolean): S
  }

  trait RotStopCommand[F[_], +S] {
    def setBrakes(enable: Boolean): S
  }

  trait RotMoveCommand[F[_], +S] {
    def setAngle(angle: RotatorAngle): S
  }

  trait CarouselModeCommand[F[_], +S] {
    def setDomeMode(mode:        DomeMode): S
    def setShutterMode(mode:     ShutterMode): S
    def setSlitHeight(height:    Double): S
    def setDomeEnable(enable:    Boolean): S
    def setShutterEnable(enable: Boolean): S
  }

  trait CarouselMoveCommand[F[_], +S] {
    def setAngle(angle: Angle): S
  }

  trait ShuttersMoveCommand[F[_], +S] {
    def setTop(pos:    Double): S
    def setBottom(pos: Double): S
  }

  trait VentGatesMoveCommand[F[_], +S] {
    def setVentGateEast(pos: Double): S
    def setVentGateWest(pos: Double): S
  }

  trait TargetCommand[F[_], +S] {
    def objectName(v:     String): S
    def coordSystem(v:    String): S
    def coord1(v:         String): S
    def coord2(v:         String): S
    def epoch(v:          Double): S
    def equinox(v:        String): S
    def parallax(v:       Double): S
    def properMotion1(v:  Double): S
    def properMotion2(v:  Double): S
    def radialVelocity(v: Double): S
    def brightness(v:     Double): S
    def ephemerisFile(v:  String): S
  }

  trait WavelengthCommand[F[_], +S] {
    def wavelength(v: Wavelength): S
  }

  trait SlewOptionsCommand[F[_], +S] {
    def zeroChopThrow(v:            Boolean): S
    def zeroSourceOffset(v:         Boolean): S
    def zeroSourceDiffTrack(v:      Boolean): S
    def zeroMountOffset(v:          Boolean): S
    def zeroMountDiffTrack(v:       Boolean): S
    def shortcircuitTargetFilter(v: Boolean): S
    def shortcircuitMountFilter(v:  Boolean): S
    def resetPointing(v:            Boolean): S
    def stopGuide(v:                Boolean): S
    def zeroGuideOffset(v:          Boolean): S
    def zeroInstrumentOffset(v:     Boolean): S
    def autoparkPwfs1(v:            Boolean): S
    def autoparkPwfs2(v:            Boolean): S
    def autoparkOiwfs(v:            Boolean): S
    def autoparkGems(v:             Boolean): S
    def autoparkAowfs(v:            Boolean): S
  }

  trait RotatorCommand[F[_], +S] {
    def ipa(v:     Angle): S
    def system(v:  String): S
    def equinox(v: String): S
    def iaa(v:     Angle): S
  }

  trait OriginCommand[F[_], +S] {
    def originX(v: Angle): S
    def originY(v: Angle): S
  }

  trait FocusOffsetCommand[F[_], +S] {
    def focusOffset(v: Distance): S
  }

  trait InstrumentOffsetCommand[F[_], +S] {
    def offsetX(v: Distance): S
    def offsetY(v: Distance): S
  }

  trait WrapsCommand[F[_], +S] {
    def azimuth(v: Int): S
    def rotator(v: Int): S
  }

  trait ProbeTrackingCommand[F[_], +S] {
    def nodAchopA(v: Boolean): S
    def nodAchopB(v: Boolean): S
    def nodBchopA(v: Boolean): S
    def nodBchopB(v: Boolean): S
  }

  trait ProbeCommands[F[_], +S] {
    val park: BaseCommand[F, TcsCommands[F]]
    val follow: FollowCommand[F, TcsCommands[F]]
  }

  trait MoveCommand[F[_], A, +S] {
    def setPosition(v: A): S
  }

  trait AgMechCommands[F[_], A, +S] {
    val park: BaseCommand[F, TcsCommands[F]]
    val move: MoveCommand[F, A, TcsCommands[F]]
  }

  trait GuideCommand[F[_], +S] {
    def state(v: Boolean): S
  }

  trait M1GuideConfigCommand[F[_], +S] {
    def weighting(v: String): S
    def source(v:    String): S
    def frames(v:    Int): S
    def filename(v:  String): S
  }

  trait M2GuideModeCommand[F[_], +S] {
    def coma(v: Boolean): S
  }

  trait M2GuideConfigCommand[F[_], +S] {
    def source(v:     String): S
    def sampleFreq(v: Double): S
    def filter(v:     String): S
    def freq1(v:      Option[Double]): S
    def freq2(v:      Option[Double]): S
    def beam(v:       String): S
    def reset(v:      Boolean): S
  }

  trait MountGuideCommand[F[_], +S] {
    def mode(v:     Boolean): S
    def source(v:   String): S
    def p1Weight(v: Double): S
    def p2Weight(v: Double): S
  }

  trait BafflesCommand[F[_], +S] {
    def central(v:    CentralBafflePosition): S
    def deployable(v: DeployableBafflePosition): S
  }

  trait WfsObserveCommand[F[_], +S] {
    def numberOfExposures(v: Int): S
    def interval(v:          Double): S
    def options(v:           String): S
    def label(v:             String): S
    def output(v:            String): S
    def path(v:              String): S
    def fileName(v:          String): S
  }

  trait ProbeGuideModeCommand[F[_], +S] {
    def setMode(pg: Option[ProbeGuide]): S
  }

  trait WfsSignalProcConfigCommand[F[_], +S] {
    def darkFilename(v: String): S
  }

  trait WfsDarkCommand[F[_], +S] {
    def filename(v: String): S
  }

  trait WfsClosedLoopCommand[F[_], +S] {
    def global(v:      Double): S
    def average(v:     Int): S
    def zernikes2m2(v: Int): S
    def mult(v:        Double): S
  }

  trait WfsProcCommands[F[_], +S] {
    val signalProc: WfsSignalProcConfigCommand[F, S]
    val dark: WfsDarkCommand[F, S]
    val closedLoop: WfsClosedLoopCommand[F, S]
  }

  trait OiwfsSelectCommand[F[_], +S] {
    def oiwfsName(v: String): S
    def output(v:    String): S
  }

  trait M1Commands[F[_], +S] {
    def park: S
    def unpark: S
    def figureUpdates(enable: Boolean): S
    def zero(mech:            String): S
    def saveModel(name:       String): S
    def loadModel(name:       String): S
    def ao(enable:            Boolean): S
  }

  trait AdjustCommand[F[_], +S] {
    def frame(frm:  ReferenceFrame): S
    def size(sz:    Double): S
    def angle(a:    Angle): S
    def vtMask(vts: List[VirtualTelescope]): S
  }

  trait OffsetMgmCommand[F[_], +S] {
    def vt(v:    VirtualTelescope): S
    def index(v: OffsetIndexSelection): S
  }

  trait TargetFilterCommand[F[_], +S] {
    def bandwidth(bw:    Double): S
    def maxVelocity(mv:  Double): S
    def grabRadius(gr:   Double): S
    def shortcircuit(sc: ShortcircuitTargetFilter.Type): S
  }

  trait PointingAdjustCommand[F[_], +S] {
    def frame(frm: ReferenceFrame): S
    def size(sz:   Double): S
    def angle(a:   Angle): S
  }

  trait PointingConfigCommand[F[_], +S] {
    def name(nm:  PointingParameter): S
    def level(lv: PointingConfigLevel): S
    def value(v:  Double): S
  }

  trait ChopConfigCommand[F[_]] {
    def typ(tp:         String): TcsCommands[F]
    def source(src:     ChopSource): TcsCommands[F]
    def frequency(f:    Double): TcsCommands[F]
    def dutyCycle(duty: Double): TcsCommands[F]
  }

  trait ChopRelativeCommand[F[_]] {
    def thrw(d:     Angle): TcsCommands[F]
    def angle(a:    Angle): TcsCommands[F]
    def system(sys: String): TcsCommands[F]
    def equinox(eq: String): TcsCommands[F]
  }

  trait TcsCommands[F[_]] {
    def post: VerifiedEpics[F, F, ApplyCommandResult]
    val mcsParkCommand: BaseCommand[F, TcsCommands[F]]
    val mcsFollowCommand: FollowCommand[F, TcsCommands[F]]
    val rotStopCommand: RotStopCommand[F, TcsCommands[F]]
    val rotParkCommand: BaseCommand[F, TcsCommands[F]]
    val rotFollowCommand: FollowCommand[F, TcsCommands[F]]
    val rotMoveCommand: RotMoveCommand[F, TcsCommands[F]]
    val ecsCarouselModeCmd: CarouselModeCommand[F, TcsCommands[F]]
    val ecsCarouselMoveCmd: CarouselMoveCommand[F, TcsCommands[F]]
    val ecsShuttersMoveCmd: ShuttersMoveCommand[F, TcsCommands[F]]
    val ecsVenGatesMoveCmd: VentGatesMoveCommand[F, TcsCommands[F]]
    val sourceACmd: TargetCommand[F, TcsCommands[F]]
    val pwfs1TargetCmd: TargetCommand[F, TcsCommands[F]]
    val pwfs2TargetCmd: TargetCommand[F, TcsCommands[F]]
    val oiwfsTargetCmd: TargetCommand[F, TcsCommands[F]]
    val sourceAWavel: WavelengthCommand[F, TcsCommands[F]]
    val pwfs1Wavel: WavelengthCommand[F, TcsCommands[F]]
    val pwfs2Wavel: WavelengthCommand[F, TcsCommands[F]]
    val oiwfsWavel: WavelengthCommand[F, TcsCommands[F]]
    val slewOptionsCommand: SlewOptionsCommand[F, TcsCommands[F]]
    val rotatorCommand: RotatorCommand[F, TcsCommands[F]]
    val originCommand: OriginCommand[F, TcsCommands[F]]
    val focusOffsetCommand: FocusOffsetCommand[F, TcsCommands[F]]
    val pwfs1ProbeTrackingCommand: ProbeTrackingCommand[F, TcsCommands[F]]
    val pwfs1ProbeCommands: ProbeCommands[F, TcsCommands[F]]
    val pwfs2ProbeTrackingCommand: ProbeTrackingCommand[F, TcsCommands[F]]
    val pwfs2ProbeCommands: ProbeCommands[F, TcsCommands[F]]
    val oiwfsProbeTrackingCommand: ProbeTrackingCommand[F, TcsCommands[F]]
    val oiwfsProbeCommands: ProbeCommands[F, TcsCommands[F]]
    val m1GuideCommand: GuideCommand[F, TcsCommands[F]]
    val m1GuideConfigCommand: M1GuideConfigCommand[F, TcsCommands[F]]
    val m2GuideCommand: GuideCommand[F, TcsCommands[F]]
    val m2GuideModeCommand: M2GuideModeCommand[F, TcsCommands[F]]
    val m2GuideConfigCommand: M2GuideConfigCommand[F, TcsCommands[F]]
    val m2GuideResetCommand: BaseCommand[F, TcsCommands[F]]
    val m2FollowCommand: FollowCommand[F, TcsCommands[F]]
    val mountGuideCommand: MountGuideCommand[F, TcsCommands[F]]
    val pwfs1Commands: WfsProcCommands[F, TcsCommands[F]]
    val pwfs2Commands: WfsProcCommands[F, TcsCommands[F]]
    val probeGuideModeCommand: ProbeGuideModeCommand[F, TcsCommands[F]]
    val oiwfsSelectCommand: OiwfsSelectCommand[F, TcsCommands[F]]
    val bafflesCommand: BafflesCommand[F, TcsCommands[F]]
    val hrwfsCommands: AgMechCommands[F, HrwfsPickupPosition, TcsCommands[F]]
    val scienceFoldCommands: AgMechCommands[F, ScienceFold.Position, TcsCommands[F]]
    val aoFoldCommands: AgMechCommands[F, AoFoldPosition, TcsCommands[F]]
    val m1Commands: M1Commands[F, TcsCommands[F]]
    val targetAdjustCommand: AdjustCommand[F, TcsCommands[F]]
    val targetOffsetAbsorb: OffsetMgmCommand[F, TcsCommands[F]]
    val targetOffsetClear: OffsetMgmCommand[F, TcsCommands[F]]
    val originAdjustCommand: AdjustCommand[F, TcsCommands[F]]
    val originOffsetAbsorb: OffsetMgmCommand[F, TcsCommands[F]]
    val originOffsetClear: OffsetMgmCommand[F, TcsCommands[F]]
    val targetFilter: TargetFilterCommand[F, TcsCommands[F]]
    val pointingAdjustCommand: PointingAdjustCommand[F, TcsCommands[F]]
    val pointingConfigCommand: PointingConfigCommand[F, TcsCommands[F]]
    val absorbGuideCommand: BaseCommand[F, TcsCommands[F]]
    val zeroGuideCommand: BaseCommand[F, TcsCommands[F]]
    val instrumentOffsetCommand: InstrumentOffsetCommand[F, TcsCommands[F]]
    val wrapsCommand: WrapsCommand[F, TcsCommands[F]]
    val zeroRotatorGuide: BaseCommand[F, TcsCommands[F]]
    val pwfs1MechCommands: PwfsMechCommands[F]
    val pwfs2MechCommands: PwfsMechCommands[F]
    val chopConfig: ChopConfigCommand[F]
    val chopRelative: ChopRelativeCommand[F]
  }

}
