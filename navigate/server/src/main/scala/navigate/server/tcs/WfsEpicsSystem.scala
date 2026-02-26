// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Monad
import cats.Parallel
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.refined.auto.*
import lucuma.core.util.TimeSpan
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult
import navigate.server.acm.CadDirective
import navigate.server.acm.ParameterList.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

trait WfsEpicsSystem[F[_]] {
  def startGainCommand(timeout: FiniteDuration): WfsEpicsSystem.WfsGainCommands[F]
  def getQualityStatus: WfsEpicsSystem.WfsQualityStatus[F]
  def getIntegrationTime: VerifiedEpics[F, F, TimeSpan]
  def integrationTimeStream: VerifiedEpics[F, Resource[F, *], Stream[F, TimeSpan]]
  val telltale: TelltaleChannel[F]
}

object WfsEpicsSystem {

  trait WfsGainCommands[F[_]] {
    def post: VerifiedEpics[F, F, ApplyCommandResult]
    val gains: GainsCommand[F, WfsGainCommands[F]]
    def resetGain: WfsGainCommands[F]
  }

  trait GainsCommand[F[_], +S] {
    def setTipGain(v:   Double): S
    def setTiltGain(v:  Double): S
    def setFocusGain(v: Double): S
    def setScaleGain(v: Double): S
  }

  private[tcs] def buildSystem[F[_]: {Temporal, Parallel, Dispatcher}](
    channels: WfsChannels[F]
  ): WfsEpicsSystem[F] = new WfsEpicsSystem[F] {
    override def startGainCommand(timeout: FiniteDuration): WfsGainCommands[F] =
      new WfsGainCommandsImpl[F](
        new WfsEpicsImpl[F](channels),
        timeout
      )

    override def getQualityStatus: WfsQualityStatus[F] = new WfsQualityStatus[F] {
      override val flux: VerifiedEpics[F, F, Int]                 =
        VerifiedEpics.readChannel(channels.telltale, channels.flux)
      override val centroidDetected: VerifiedEpics[F, F, Boolean] = VerifiedEpics
        .readChannel(channels.telltale, channels.centroidDetected)
        .flatMap(x => VerifiedEpics.liftF[F, F, Boolean](x.map(_ === 0)))
    }

    override val telltale: TelltaleChannel[F] = channels.telltale

    override def getIntegrationTime: VerifiedEpics[F, F, TimeSpan] = VerifiedEpics
      .readChannel(channels.telltale, channels.integrationTime)
      .map(_.map(x => TimeSpan.fromSeconds(x).getOrElse(TimeSpan.Zero)))

    override def integrationTimeStream: VerifiedEpics[F, Resource[F, *], Stream[F, TimeSpan]] =
      VerifiedEpics
        .eventStream(channels.telltale, channels.integrationTime)
        .map(
          _.map(
            _.collect { case StreamEvent.ValueChanged(t) =>
              TimeSpan.fromSeconds(t).getOrElse(TimeSpan.Zero)
            }
          )
        )
  }

  def build[F[_]: {Temporal, Parallel, Dispatcher}](
    service:             EpicsService[F],
    sysName:             String,
    top:                 NonEmptyString,
    gainResetName:       NonEmptyString = "dc:initSigInit.J".refined,
    fluxName:            NonEmptyString = "dc:fgDiag1PW.VALQ".refined,
    centroidName:        NonEmptyString = "dc:fgDiag1PW.VALB".refined,
    telltaleChannelName: NonEmptyString = "health.VAL".refined
  ): Resource[F, WfsEpicsSystem[F]] = for {
    channels <-
      WfsChannels.build(service,
                        sysName,
                        top,
                        telltaleChannelName,
                        gainResetName,
                        fluxName,
                        centroidName
      )
  } yield buildSystem(channels)

  trait WfsEpics[F[_]] {
    def post(timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult]
    def clear: VerifiedEpics[F, F, Unit]
    val gainsCmd: Command4Channels[F, Double, Double, Double, Double]
    val resetGainCmd: VerifiedEpics[F, F, Unit]
  }

  // There is no CAR associated these commands. Without feedback, we can only wait and pray.
  val commandWaitTime: FiniteDuration = FiniteDuration(500, TimeUnit.MILLISECONDS)

  class WfsEpicsImpl[F[_]: Temporal](
    channels: WfsChannels[F]
  ) extends WfsEpics[F] {
    override def post(timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult] =
      VerifiedEpics.writeChannel(channels.telltale, channels.gainsDir)(
        CadDirective.START.pure[F]
      ) *>
        VerifiedEpics.liftF(Temporal[F].sleep(commandWaitTime).as(ApplyCommandResult.Completed))

    override def clear: VerifiedEpics[F, F, Unit] =
      VerifiedEpics.writeChannel(channels.telltale, channels.gainsDir)(
        CadDirective.CLEAR.pure[F]
      )

    override val gainsCmd: Command4Channels[F, Double, Double, Double, Double] =
      Command4Channels[F, Double, Double, Double, Double](
        channels.telltale,
        channels.tipGain,
        channels.tiltGain,
        channels.focusGain,
        channels.scaleGain
      )
    override val resetGainCmd: VerifiedEpics[F, F, Unit]                       = writeChannel[F, Double](
      channels.telltale,
      channels.reset
    )(1.0.pure[F])
  }

  private[tcs] class WfsGainCommandsImpl[F[_]: {Monad, Parallel}](
    wfsEpics: WfsEpics[F],
    timeout:  FiniteDuration,
    params:   ParameterList[F] = List.empty[VerifiedEpics[F, F, Unit]]
  ) extends WfsGainCommands[F] {
    private def addParam(p: VerifiedEpics[F, F, Unit]): WfsGainCommands[F] =
      WfsGainCommandsImpl(wfsEpics, timeout, params :+ p)

    override def post: VerifiedEpics[F, F, ApplyCommandResult] =
      wfsEpics.clear *> params.compile *> wfsEpics.post(timeout)

    override val gains: GainsCommand[F, WfsGainCommands[F]] =
      new GainsCommand[F, WfsGainCommands[F]] {
        override def setTipGain(v: Double): WfsGainCommands[F] = addParam(
          wfsEpics.gainsCmd.setParam1(v)
        )

        override def setTiltGain(v: Double): WfsGainCommands[F] = addParam(
          wfsEpics.gainsCmd.setParam2(v)
        )

        override def setFocusGain(v: Double): WfsGainCommands[F] = addParam(
          wfsEpics.gainsCmd.setParam3(v)
        )

        override def setScaleGain(v: Double): WfsGainCommands[F] = addParam(
          wfsEpics.gainsCmd.setParam4(v)
        )
      }
    override def resetGain: WfsGainCommands[F]              = addParam(wfsEpics.resetGainCmd)
  }

  trait WfsQualityStatus[F[_]] {
    val flux: VerifiedEpics[F, F, Int]
    val centroidDetected: VerifiedEpics[F, F, Boolean]
  }

}
