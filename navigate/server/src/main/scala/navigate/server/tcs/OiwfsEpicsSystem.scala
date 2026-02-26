// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.refined.auto.*
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult
import navigate.server.acm.*
import navigate.server.acm.ParameterList.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

trait OiwfsEpicsSystem[F[_]] extends WfsEpicsSystem[F] {
  def startDarkCommand(timeout:       FiniteDuration): OiwfsEpicsSystem.DarkCommand[F]
  def startClosedLoopCommand(timeout: FiniteDuration): OiwfsEpicsSystem.ClosedLoopCommand[F]
  def startSignalProcCommand(timeout: FiniteDuration): OiwfsEpicsSystem.SignalProcCommand[F]
}

object OiwfsEpicsSystem {

  trait DarkCommand[F[_]] {
    def post: VerifiedEpics[F, F, ApplyCommandResult]
    def filename(name: String): DarkCommand[F]
  }

  trait ClosedLoopCommand[F[_]] {
    def post: VerifiedEpics[F, F, ApplyCommandResult]
    def zernikes2m2(enable: Int): ClosedLoopCommand[F]
  }

  trait SignalProcCommand[F[_]] {
    def post: VerifiedEpics[F, F, ApplyCommandResult]
    def filename(name: String): SignalProcCommand[F]
  }

  val commandWaitTime: FiniteDuration = FiniteDuration(500, TimeUnit.MILLISECONDS)

  private class DarkCommandImpl[F[_]: {Temporal, Parallel}](
    telltale: TelltaleChannel[F],
    channels: OiwfsChannels[F],
    timeout:  FiniteDuration,
    params:   ParameterList[F] = List.empty[VerifiedEpics[F, F, Unit]]
  ) extends DarkCommand[F] {
    private def addParam(p: VerifiedEpics[F, F, Unit]): DarkCommand[F] =
      DarkCommandImpl(telltale, channels, timeout, params :+ p)

    override def post: VerifiedEpics[F, F, ApplyCommandResult] =
      writeChannel(telltale, channels.detSigModeSeqDarkDir)(
        CadDirective.CLEAR.pure[F]
      ) *> params.compile *> writeChannel(telltale, channels.detSigModeSeqDarkDir)(
        CadDirective.START.pure[F]
      ) *>
        VerifiedEpics.liftF(Temporal[F].sleep(commandWaitTime).as(ApplyCommandResult.Completed))

    override def filename(name: String): DarkCommand[F] = addParam(
      writeCadParam(telltale, channels.seqDarkFilename)(name)
    )
  }

  private class ClosedLoopCommandImpl[F[_]: {Temporal, Parallel}](
    telltale: TelltaleChannel[F],
    channels: OiwfsChannels[F],
    timeout:  FiniteDuration,
    params:   ParameterList[F] = List.empty[VerifiedEpics[F, F, Unit]]
  ) extends ClosedLoopCommand[F] {
    private def addParam(p: VerifiedEpics[F, F, Unit]): ClosedLoopCommand[F] =
      ClosedLoopCommandImpl(telltale, channels, timeout, params :+ p)

    override def post: VerifiedEpics[F, F, ApplyCommandResult] =
      writeChannel(telltale, channels.detSigModeSeqDir)(
        CadDirective.CLEAR.pure[F]
      ) *>
        params.compile *> writeChannel(telltale, channels.detSigModeSeqDir)(
          CadDirective.START.pure[F]
        ) *>
        VerifiedEpics.liftF(Temporal[F].sleep(commandWaitTime).as(ApplyCommandResult.Completed))

    override def zernikes2m2(enable: Int): ClosedLoopCommand[F] = addParam(
      writeCadParam(telltale, channels.z2m2)(enable)
    )
  }

  private class SignalProcCommandImpl[F[_]: {Temporal, Parallel}](
    telltale: TelltaleChannel[F],
    channels: OiwfsChannels[F],
    timeout:  FiniteDuration,
    params:   ParameterList[F] = List.empty[VerifiedEpics[F, F, Unit]]
  ) extends SignalProcCommand[F] {
    private def addParam(p: VerifiedEpics[F, F, Unit]): SignalProcCommand[F] =
      SignalProcCommandImpl(telltale, channels, timeout, params :+ p)

    override def post: VerifiedEpics[F, F, ApplyCommandResult] =
      writeChannel(telltale, channels.detSigInitDir)(
        CadDirective.CLEAR.pure[F]
      ) *>
        params.compile *> writeChannel(telltale, channels.detSigInitDir)(
          CadDirective.START.pure[F]
        ) *>
        VerifiedEpics.liftF(Temporal[F].sleep(commandWaitTime).as(ApplyCommandResult.Completed))

    override def filename(name: String): SignalProcCommand[F] = addParam(
      writeCadParam(telltale, channels.darkFilename)(name)
    )
  }

  private[tcs] def buildSystem[F[_]: {Temporal, Parallel, Dispatcher}](
    wfsChannels:   WfsChannels[F],
    oiwfsChannels: OiwfsChannels[F]
  ): OiwfsEpicsSystem[F] = new OiwfsEpicsSystem[F] {
    private val wfsEpicsSystem: WfsEpicsSystem[F] = WfsEpicsSystem.buildSystem[F](wfsChannels)

    export wfsEpicsSystem.{
      getIntegrationTime,
      getQualityStatus,
      integrationTimeStream,
      startGainCommand
    }

    override def startDarkCommand(timeout: FiniteDuration): DarkCommand[F] =
      new DarkCommandImpl[F](wfsChannels.telltale, oiwfsChannels, timeout)

    override def startClosedLoopCommand(timeout: FiniteDuration): ClosedLoopCommand[F] =
      ClosedLoopCommandImpl[F](wfsChannels.telltale, oiwfsChannels, timeout)

    override def startSignalProcCommand(timeout: FiniteDuration): SignalProcCommand[F] =
      SignalProcCommandImpl[F](wfsChannels.telltale, oiwfsChannels, timeout)

    override val telltale: TelltaleChannel[F] = wfsEpicsSystem.telltale
  }

  val systemName: String = "OIWFS"

  def build[F[_]: {Temporal, Parallel, Dispatcher}](
    service:      EpicsService[F],
    top:          NonEmptyString,
    fluxName:     NonEmptyString,
    centroidName: NonEmptyString
  ): Resource[F, OiwfsEpicsSystem[F]] = for {
    wfsc <- WfsChannels.build(service,
                              systemName,
                              top,
                              "dc:seeing.VAL".refined,
                              "dc:initSigInitFgGain.PROC".refined,
                              fluxName,
                              centroidName
            )
    oic  <- OiwfsChannels.build(service, top)
  } yield buildSystem(wfsc, oic)

}
