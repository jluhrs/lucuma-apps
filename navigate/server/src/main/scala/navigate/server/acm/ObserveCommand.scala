// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.Applicative
import cats.Eq
import cats.derived.*
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.effect.syntax.temporal.*
import cats.syntax.all.*
import fs2.RaiseThrowable.*
import fs2.Stream
import mouse.all.booleanSyntaxMouse
import navigate.epics.Channel
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult
import navigate.server.epicsdata.BinaryYesNo

import scala.concurrent.duration.*

trait ObserveCommand[F[_]] {
  import ObserveCommand.CommandType

  /**
   * Given a pair of apply and CAR records and a observing status, this function produces a program
   * that will trigger the apply record and then monitor the apply and CAR record to finally produce
   * a command result. If the command takes longer than the <code>timeout</code>, it will produce an
   * error. The program is contained in a <code>VerifiedEpics</code> that checks the connection to
   * the channels.
   * @param timeout
   *   The timeout for running the command
   * @return
   */
  def post(typ: CommandType, timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult]

  def clearIfNotBusy: VerifiedEpics[F, F, Unit]
}

object ObserveCommand {

  enum CommandType derives Eq {
    case PermanentOn  extends CommandType
    case PermanentOff extends CommandType
    case TemporarlyOn extends CommandType
    case NoWait       extends CommandType
  }

  enum CmdPhase {
    case WaitingBusy extends CmdPhase
    case WaitingIdle extends CmdPhase
    case Done        extends CmdPhase
  }

  enum ObsPhase {
    case WaitingOn  extends ObsPhase
    case WaitingOff extends ObsPhase
    case Done       extends ObsPhase
  }

  case class PostState(applyVal: Option[Int], cmdPhase: CmdPhase, obsPhase: ObsPhase)

  sealed trait Event                              extends Product with Serializable
  case class ApplyValChange(v: Int)               extends Event
  case class CarValChange(v: CarState, clid: Int) extends Event
  case class IntegratingValChange(v: BinaryYesNo) extends Event

  // Delay reading error messages by this amount after an error is triggered
  private val MessageReadDelay: FiniteDuration = 100.milliseconds

  private final class ObserveCommandImpl[F[_]: {Dispatcher, Temporal}](
    telltaleChannel: TelltaleChannel[F],
    apply:           ApplyRecord[F],
    car:             CarRecord[F],
    integrating:     Channel[F, BinaryYesNo]
  ) extends ObserveCommand[F] {
    override def post(
      typ:     CommandType,
      timeout: FiniteDuration
    ): VerifiedEpics[F, F, ApplyCommandResult] = {
      val streamsV = for {
        avrs   <- eventStream(telltaleChannel, apply.oval)
        cvrs   <- eventStream(telltaleChannel, car.oval)
        dwr    <- writeChannel(telltaleChannel, apply.dir)(Applicative[F].pure(CadDirective.START))
                    .map(Resource.pure[F, F[Unit]])
        msrr   <- readChannel(telltaleChannel, apply.mess)
                    .map(Temporal[F].delayBy(_, MessageReadDelay))
                    .map(Resource.pure[F, F[String]])
        omsrr  <- readChannel(telltaleChannel, car.omss)
                    .map(Temporal[F].delayBy(_, MessageReadDelay))
                    .map(Resource.pure[F, F[String]])
        clidrr <- readChannel(telltaleChannel, car.clid).map(Resource.pure[F, F[Int]])
        avrr   <- readChannel(telltaleChannel, apply.oval).map(Resource.pure[F, F[Int]])
        cvrr   <- readChannel(telltaleChannel, car.oval).map(Resource.pure[F, F[CarState]])
        intsF  <- eventStream(telltaleChannel, integrating)
      } yield for {
        avs   <- avrs
        cvs   <- cvrs
        dw    <- dwr
        msr   <- msrr
        omsr  <- omsrr
        clidr <- clidrr
        avr   <- avrr
        cvr   <- cvrr
        ints  <- intsF
      } yield (avs, cvs, dw, msr, omsr, clidr, avr, cvr, ints)

      streamsV.map(_.use { case (avs, cvs, dw, msr, omsr, clidr, avr, cvr, ints) =>
        processCommand(typ, avs, cvs, dw, msr, omsr, clidr, avr, cvr, ints).timeout(timeout)
      })

    }

    /**
     * processCommand is the heart of the observe command processing. It implements a state machine
     * inside a Stream, that follows the algorithm described in Gemini ICD 1b, section 6.1, while
     * also monitoring the integration state of the WFS. The events of the state machine are changes
     * on the EPICS channels apply.VAL, car.VAL and the integration state channel. The monitors
     * created by the CA library always give the current value as the first event (if there is a
     * current value). That holds true for the Streams created from those monitors. That is the
     * reason that apply.VAL is read at the beginning, to compare the initial value with the stream
     * values and recognize actual changes.
     *
     * @param typ
     *   : Observe operation type.
     * @param avs
     *   : Stream of values from apply.VAL
     * @param cvs
     *   : Stream of values from car.VAL
     * @param dw
     *   : Effect that will write a START on apply.DIR when evaluated.
     * @param msr
     *   : Effect to read apply.MESS
     * @param omsr
     *   : Effect to read car.OMSS
     * @param clidr
     *   : Effect to read car.CLID
     * @param avr
     *   : Effect to read apply.VAL
     * @param avr
     *   : Effect to read car.VAL
     * @param ints
     *   : Stream of values from the integration state channel
     * @return
     *   Effect that return the command result on completion.
     */
    private def processCommand(
      typ:   CommandType,
      avs:   Stream[F, StreamEvent[Int]],
      cvs:   Stream[F, StreamEvent[CarState]],
      dw:    F[Unit],
      msr:   F[String],
      omsr:  F[String],
      clidr: F[Int],
      avr:   F[Int],
      cvr:   F[CarState],
      ints:  Stream[F, StreamEvent[BinaryYesNo]]
    ): F[ApplyCommandResult] = {
      val startObsPhase = typ match {
        case CommandType.PermanentOn  => ObsPhase.WaitingOn
        case CommandType.PermanentOff => ObsPhase.WaitingOff
        case CommandType.TemporarlyOn => ObsPhase.WaitingOn
        case CommandType.NoWait       => ObsPhase.Done
      }

      def processApplyChange(v: Int, s: PostState): (PostState, F[Option[ApplyCommandResult]]) =
        s match {
          case u @ PostState(None, _, _) if v < 0   =>
            (
              u,
              msr.flatMap(msg =>
                new Throwable(s"Apply record ${apply.name} failed with error: $msg").raiseError
              )
            )
          case u @ PostState(None, _, _) if v === 0 =>
            (
              u,
              new Throwable(
                s"Apply record ${apply.name} triggered externally while processing."
              ).raiseError
            )
          case PostState(None, b, c)                =>
            (PostState(v.some, b, c), none[ApplyCommandResult].pure[F])
          case u                                    =>
            (u, none[ApplyCommandResult].pure[F])
        }

      def processCarChange(
        v:    CarState,
        clid: Int,
        s:    PostState
      ): (PostState, F[Option[ApplyCommandResult]]) = s match {
        case PostState(Some(av), CmdPhase.WaitingIdle, ObsPhase.Done)
            if v === CarState.IDLE && clid >= av =>
          (
            PostState(Some(av), CmdPhase.Done, ObsPhase.Done),
            ApplyCommandResult.Completed.some.pure[F]
          )
        case PostState(Some(av), CmdPhase.WaitingIdle, c) if v === CarState.IDLE && clid >= av =>
          (
            PostState(Some(av), CmdPhase.Done, c),
            none[ApplyCommandResult].pure[F]
          )
        case PostState(a, CmdPhase.WaitingIdle, c) if v === CarState.IDLE                      =>
          (
            PostState(a, CmdPhase.WaitingBusy, c),
            none[ApplyCommandResult].pure[F]
          )
        case PostState(a, CmdPhase.WaitingBusy, c) if v === CarState.BUSY                      =>
          (
            PostState(a, CmdPhase.WaitingIdle, c),
            none[ApplyCommandResult].pure[F]
          )
        case PostState(Some(av), CmdPhase.WaitingIdle, ObsPhase.Done)
            if v === CarState.ERROR && clid >= av =>
          (
            PostState(Some(av), CmdPhase.Done, ObsPhase.Done),
            omsr.flatMap(msg => new Throwable(s"CAR record ${car.name} has error: $msg").raiseError)
          )
        case u                                                                                 =>
          (
            u,
            none[ApplyCommandResult].pure[F]
          )
      }

      def processIntegratingChange(
        v: BinaryYesNo,
        s: PostState
      ): (PostState, F[Option[ApplyCommandResult]]) = typ match {
        case CommandType.PermanentOn  =>
          s match {
            case PostState(a, CmdPhase.Done, ObsPhase.WaitingOn) if v === BinaryYesNo.Yes =>
              (
                PostState(a, CmdPhase.Done, ObsPhase.Done),
                ApplyCommandResult.Completed.some.pure[F]
              )
            case PostState(a, b, ObsPhase.WaitingOn) if v === BinaryYesNo.Yes             =>
              (
                PostState(a, b, ObsPhase.Done),
                none[ApplyCommandResult].pure[F]
              )
            case PostState(a, b, _) if v === BinaryYesNo.No                               =>
              (
                PostState(a, b, ObsPhase.WaitingOn),
                none[ApplyCommandResult].pure[F]
              )
            case u                                                                        =>
              (
                u,
                none[ApplyCommandResult].pure[F]
              )
          }
        case CommandType.PermanentOff =>
          s match {
            case PostState(a, CmdPhase.Done, ObsPhase.WaitingOff) if v === BinaryYesNo.No =>
              (
                PostState(a, CmdPhase.Done, ObsPhase.Done),
                ApplyCommandResult.Completed.some.pure[F]
              )
            case PostState(a, b, ObsPhase.WaitingOff) if v === BinaryYesNo.No             =>
              (
                PostState(a, b, ObsPhase.Done),
                none[ApplyCommandResult].pure[F]
              )
            case PostState(a, b, _) if v === BinaryYesNo.Yes                              =>
              (
                PostState(a, b, ObsPhase.WaitingOff),
                none[ApplyCommandResult].pure[F]
              )
            case u                                                                        =>
              (
                u,
                none[ApplyCommandResult].pure[F]
              )
          }
        case CommandType.TemporarlyOn =>
          s match {
            case PostState(a, CmdPhase.Done, ObsPhase.WaitingOff) if v === BinaryYesNo.No =>
              (
                PostState(a, CmdPhase.Done, ObsPhase.Done),
                ApplyCommandResult.Completed.some.pure[F]
              )
            case PostState(a, b, ObsPhase.WaitingOff) if v === BinaryYesNo.No             =>
              (
                PostState(a, b, ObsPhase.Done),
                none[ApplyCommandResult].pure[F]
              )
            case PostState(a, b, _) if v === BinaryYesNo.Yes                              =>
              (
                PostState(a, b, ObsPhase.WaitingOff),
                none[ApplyCommandResult].pure[F]
              )
            case u                                                                        =>
              (
                u,
                none[ApplyCommandResult].pure[F]
              )
          }
        case CommandType.NoWait       =>
          s match {
            case u @ PostState(_, CmdPhase.Done, _) =>
              // This should be an unreachable case
              (
                u,
                new Throwable(
                  s"Observe post should have already exited (Apply record ${apply.name}, integrating channel ${integrating.getName}."
                ).raiseError
              )
            case u                                  =>
              (
                u,
                none[ApplyCommandResult].pure[F]
              )
          }
      }

      (for {
        av0 <- Stream.eval(avr.attempt.map(_.toOption))
        cv0 <- Stream.eval(cvr.attempt.map(_.toOption))
        _   <- Stream.eval(dw)
        r   <- Stream[F, Stream[F, Event]](
                 removeRepeated(avs,
                                av0,
                                s"Apply record ${apply.name} disconnected",
                                ApplyValChange.apply(_).pure[F]
                 ),
                 removeRepeated(cvs,
                                cv0,
                                s"CAR record ${apply.name} disconnected",
                                v => clidr.attempt.map(_.getOrElse(0)).map(CarValChange(v, _))
                 ),
                 removeRepeated(ints,
                                none,
                                s"Integrating channel ${integrating.getName} disconnected",
                                IntegratingValChange.apply(_).pure[F]
                 )
               ).parJoin(3)
      } yield r)
        .mapAccumulate[PostState, F[Option[ApplyCommandResult]]](
          PostState(None, CmdPhase.WaitingBusy, startObsPhase)
        ) { (s, ev) =>
          ev match {
            case ApplyValChange(v)       => processApplyChange(v, s)
            case CarValChange(v, clid)   => processCarChange(v, clid, s)
            case IntegratingValChange(v) => processIntegratingChange(v, s)
          }
        }
        .evalMap(_._2)
        .unNone
        .take(1)
        .compile
        .lastOrError
    }

    private def removeRepeated[U: Eq](
      strm:          Stream[F, StreamEvent[U]],
      v0:            Option[U],
      disconnectMsg: String,
      out:           U => F[Event]
    ): Stream[F, Event] =
      strm
        .flatMap {
          case StreamEvent.ValueChanged(v) => Stream(v)
          case StreamEvent.Disconnected    => Stream.raiseError[F](new Throwable(disconnectMsg))
          case _                           => Stream.empty
        }
        .evalMapAccumulate(v0) { case (acc, v) =>
          out(v).map(x => (v.some, acc.forall(_ =!= v).option(x)))
        }
        .map(_._2)
        .unNone

    private val ClearDelay                                 = 50.milliseconds
    override def clearIfNotBusy: VerifiedEpics[F, F, Unit] =
      readChannel(telltaleChannel, car.oval).flatMap { g =>
        VerifiedEpics.ifF(g.map(_ != CarState.BUSY))(
          writeChannel(telltaleChannel, apply.dir)(
            Applicative[F].pure(CadDirective.CLEAR)
          ) *> VerifiedEpics.liftF(Temporal[F].sleep(ClearDelay))
        )(
          VerifiedEpics.unit
        )
      }

  }

  def build[F[_]: {Dispatcher, Temporal}](
    srv:             EpicsService[F],
    telltaleChannel: TelltaleChannel[F],
    applyName:       String,
    carName:         String,
    integrating:     Channel[F, BinaryYesNo]
  ): Resource[F, ObserveCommand[F]] = for {
    apply <- ApplyRecord.build(srv, applyName)
    car   <- CarRecord.build(srv, carName)
  } yield new ObserveCommandImpl[F](telltaleChannel, apply, car, integrating)

}
