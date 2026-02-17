// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s

import cats.data.Validated
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.syntax.all.*
import ch.qos.logback.classic.spi.ILoggingEvent
import edu.gemini.schema.util.SchemaStitcher
import edu.gemini.schema.util.SourceResolver
import fs2.Stream
import fs2.concurrent.Topic
import grackle.Env
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.QueryCompiler.SelectElaborator
import grackle.Result
import grackle.Schema
import grackle.TypeRef
import grackle.Value
import grackle.Value.*
import grackle.circe.CirceMapping
import grackle.syntax.given
import io.circe.syntax.*
import lucuma.core.enums.ComaOption
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.M1Source
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Ephemeris
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.Observation
import lucuma.core.model.ProbeGuide
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Enums.EphemerisKeyType
import mouse.boolean.given
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.AcquisitionAdjustment
import navigate.model.AutoparkAowfs
import navigate.model.AutoparkGems
import navigate.model.AutoparkOiwfs
import navigate.model.AutoparkPwfs1
import navigate.model.AutoparkPwfs2
import navigate.model.BafflesConfig
import navigate.model.BafflesState
import navigate.model.CommandResult
import navigate.model.FocalPlaneOffset
import navigate.model.FocalPlaneOffset.DeltaX
import navigate.model.FocalPlaneOffset.DeltaY
import navigate.model.GuiderConfig
import navigate.model.HandsetAdjustment
import navigate.model.InstrumentSpecifics
import navigate.model.NavigateState
import navigate.model.Origin
import navigate.model.PointingCorrections
import navigate.model.PwfsMechsState
import navigate.model.ResetPointing
import navigate.model.RotatorTrackConfig
import navigate.model.RotatorTrackingMode
import navigate.model.ServerConfiguration
import navigate.model.ShortcircuitMountFilter
import navigate.model.ShortcircuitTargetFilter
import navigate.model.SlewOptions
import navigate.model.StopGuide
import navigate.model.SwapConfig
import navigate.model.Target
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
import navigate.model.config.NavigateConfiguration
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.AcquisitionAdjustmentCommand
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.LightSource
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.VirtualTelescope
import navigate.server.NavigateEngine
import navigate.server.tcs.GuideState
import navigate.server.tcs.GuidersQualityValues
import navigate.server.tcs.TargetOffsets
import navigate.server.tcs.TelescopeState
import navigate.web.server.OcsBuildInfo

import java.nio.file.Path as JPath
import java.time.LocalDate
import scala.reflect.classTag

import encoder.given

class NavigateMappings[F[_]: Sync](
  config:                         NavigateConfiguration,
  val server:                     NavigateEngine[F],
  val logTopic:                   Topic[F, ILoggingEvent],
  val guideStateTopic:            Topic[F, GuideState],
  val guidersQualityTopic:        Topic[F, GuidersQualityValues],
  val telescopeStateTopic:        Topic[F, TelescopeState],
  val acquisitionAdjustmentTopic: Topic[F, AcquisitionAdjustment],
  val targetAdjustmentTopic:      Topic[F, TargetOffsets],
  val originAdjustmentTopic:      Topic[F, FocalPlaneOffset],
  val pointingAdjustmentTopic:    Topic[F, PointingCorrections],
  val acMechsTopic:               Topic[F, AcMechsState],
  val pwfs1MechsTopic:            Topic[F, PwfsMechsState],
  val pwfs2MechsTopic:            Topic[F, PwfsMechsState],
  val logBuffer:                  Ref[F, Seq[ILoggingEvent]]
)(
  override val schema:            Schema
) extends CirceMapping[F] {
  import NavigateMappings._

  def telescopeState: F[Result[TelescopeState]] =
    server.getTelescopeState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def guideState: F[Result[GuideState]] =
    server.getGuideState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def guidersQualityValues: F[Result[GuidersQualityValues]] =
    server.getGuidersQuality.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def navigateState: F[Result[NavigateState]] =
    server.getNavigateState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def targetAdjustmentOffsets: F[Result[TargetOffsets]]   =
    server.getTargetAdjustments.attempt.map(
      _.fold(e => Result.failure(e.getMessage), Result.success)
    )
  def originAdjustmentOffset: F[Result[FocalPlaneOffset]] =
    server.getOriginOffset.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def pointingAdjustmentOffset: F[Result[PointingCorrections]] =
    server.getPointingOffset.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def acMechsState: F[Result[AcMechsState]] =
    server.getAcMechsState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def pwfs1MechsState: F[Result[PwfsMechsState]] =
    server.getPwfs1MechsState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def pwfs2MechsState: F[Result[PwfsMechsState]] =
    server.getPwfs2MechsState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def pwfs1ConfigState: F[Result[WfsConfiguration]] = server.getPwfs1Configuration.attempt.map(
    _.fold(e => Result.failure(e.getMessage), Result.success)
  )

  def pwfs2ConfigState: F[Result[WfsConfiguration]] = server.getPwfs2Configuration.attempt.map(
    _.fold(e => Result.failure(e.getMessage), Result.success)
  )

  def oiwfsConfigState: F[Result[WfsConfiguration]] = server.getOiwfsConfiguration.attempt.map(
    _.fold(e => Result.failure(e.getMessage), Result.success)
  )

  def bafflesState: F[Result[BafflesState]] =
    server.getBafflesState.attempt.map(_.fold(e => Result.failure(e.getMessage), Result.success))

  def instrumentPort(env: Env): F[Result[Option[Int]]] =
    env
      .get[Instrument]("instrument")
      .map(ins =>
        server
          .getInstrumentPort(ins)
          .attempt
          .map(_.fold(e => Result.failure(e.getMessage), Result.success))
      )
      .getOrElse(
        Result.failure[Option[Int]]("instrumentPort parameter could not be parsed.").pure[F]
      )

  def serverVersion: F[Result[String]] = Result.success(OcsBuildInfo.version).pure[F]

  def serverConfig: F[Result[ServerConfiguration]] = Result
    .success(
      ServerConfiguration(
        OcsBuildInfo.version,
        config.site,
        config.navigateEngine.odb.toString,
        config.lucumaSSO.ssoUrl.toString
      )
    )
    .pure[F]

  def mountFollow(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map { en =>
        server
          .mcsFollow(en)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome]("mountFollow parameter could not be parsed.").pure[F]
      )

  def rotatorFollow(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map { en =>
        server
          .rotFollow(en)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure("rotatorFollow parameter could not be parsed.").pure[F]
      )

  def rotatorConfig(env: Env): F[Result[OperationOutcome]] =
    env
      .get[RotatorTrackConfig]("config")
      .map { cfg =>
        server
          .rotTrackingConfig(cfg)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure("rotatorConfig parameter could not be parsed.").pure[F]
      )

  def scsFollow(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map { en =>
        server
          .scsFollow(en)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome]("scsFollow parameter could not be parsed.").pure[F]
      )

  def instrumentSpecifics(env: Env): F[Result[OperationOutcome]] =
    env
      .get[InstrumentSpecifics]("instrumentSpecificsParams")(using classTag[InstrumentSpecifics])
      .map { isp =>
        server
          .instrumentSpecifics(isp)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result
          .failure[OperationOutcome]("InstrumentSpecifics parameters could not be parsed.")
          .pure[F]
      )

  def slew(env: Env): F[Result[OperationOutcome]] = (for {
    oid <- env.get[Option[Observation.Id]]("obsId")
    so  <- env.get[SlewOptions]("slewOptions")
    tc  <- env.get[TcsConfig]("config")
  } yield server
    .slew(so, tc, oid)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome](s"Slew parameters $env oid could not be parsed.").pure[F]
  )

  def tcsConfig(env: Env): F[Result[OperationOutcome]] =
    env
      .get[TcsConfig]("config")(using classTag[TcsConfig])
      .map { tc =>
        server
          .tcsConfig(tc)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result
          .failure[OperationOutcome]("tcsConfig parameters could not be parsed.")
          .pure[F]
      )

  def swapTarget(env: Env): F[Result[OperationOutcome]] =
    env
      .get[SwapConfig]("swapConfig")(using classTag[SwapConfig])
      .map { t =>
        server
          .swapTarget(t)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result
          .failure[OperationOutcome]("swapTarget parameters could not be parsed.")
          .pure[F]
      )

  def restoreTarget(env: Env): F[Result[OperationOutcome]] =
    env
      .get[TcsConfig]("config")(using classTag[TcsConfig])
      .map { tc =>
        server
          .restoreTarget(tc)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result
          .failure[OperationOutcome]("restoreTarget parameters could not be parsed.")
          .pure[F]
      )

  private def wfsTarget(name: String, cmd: Target => F[CommandResult])(
    env: Env
  ): F[Result[OperationOutcome]] =
    env
      .get[Target]("target")(using classTag[Target])
      .map { oi =>
        cmd(oi).attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome](s"${name}Target parameters could not be parsed.").pure[F]
      )

  private def wfsProbeTracking(name: String, cmd: TrackingConfig => F[CommandResult])(
    env: Env
  ): F[Result[OperationOutcome]] =
    env
      .get[TrackingConfig]("config")(using classTag[TrackingConfig])
      .map { tc =>
        cmd(tc).attempt
          .map(convertResult)
      }
      .getOrElse(
        Result
          .failure[OperationOutcome](s"${name}ProbeTracking parameters could not be parsed.")
          .pure[F]
      )

  def wfsFollow(name: String, cmd: Boolean => F[CommandResult])(
    env: Env
  ): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map { en =>
        cmd(en).attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome](s"${name}Follow parameter could not be parsed.").pure[F]
      )

  def wfsObserve(name: String, cmd: TimeSpan => F[CommandResult])(
    env: Env
  ): F[Result[OperationOutcome]] =
    env
      .get[TimeSpan]("period")
      .map { p =>
        cmd(p).attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome](s"${name}Observe parameter could not be parsed.").pure[F]
      )

  def wfsFilter(name: String, cmd: PwfsFilter => F[CommandResult])(
    env: Env
  ): F[Result[OperationOutcome]] =
    env
      .get[PwfsFilter]("filter")
      .map { p =>
        cmd(p).attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome](s"${name}Filter parameter could not be parsed.").pure[F]
      )

  def wfsFieldStop(name: String, cmd: PwfsFieldStop => F[CommandResult])(
    env: Env
  ): F[Result[OperationOutcome]] =
    env
      .get[PwfsFieldStop]("fieldStop")
      .map { p =>
        cmd(p).attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome](s"${name}FieldStop parameter could not be parsed.").pure[F]
      )

  def acObserve(env: Env): F[Result[OperationOutcome]] =
    env
      .get[TimeSpan]("period")
      .map { p =>
        server
          .acObserve(p)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome]("acObserve parameter could not be parsed.").pure[F]
      )

  def guideEnable(env: Env): F[Result[OperationOutcome]] =
    env
      .get[TelescopeGuideConfig]("config")
      .map { cfg =>
        server
          .enableGuide(cfg)
          .attempt
          .map(convertResult)
      }
      .getOrElse(
        Result.failure[OperationOutcome]("guideEnable parameters could not be parsed.").pure[F]
      )

  def acquisitionAdjustment(env: Env): F[Result[OperationOutcome]] =
    env
      .get[AcquisitionAdjustment]("adjustment")
      .map { adj =>
        // First publish the adjustment. if the action fails other clients will be informed anyway
        acquisitionAdjustmentTopic.publish1(adj) *>
          // Run the adjustment if the user confirms, preserve the upstream error
          (adj.command === AcquisitionAdjustmentCommand.UserConfirms)
            .valueOrPure[F, Result[OperationOutcome]](
              parameterlessCommand(server.acquisitionAdj(adj.offset, adj.iaa, adj.ipa))
            )(Result.success(OperationOutcome.success))
      }
      .getOrElse {
        Result
          .failure[OperationOutcome]("acquisitionAdjustment parameters could not be parsed.")
          .pure[F]
      }
  def wfsSky(env: Env): F[Result[OperationOutcome]]                = (for {
    wfs <- env.get[GuideProbe]("wfs")
    exp <- env.get[TimeSpan]("period")
  } yield server
    .wfsSky(wfs, exp)
    .attempt
    .map(convertResult))
    .getOrElse(Result.failure[OperationOutcome]("WFS Sky parameters could not be parsed.").pure[F])

  def lightpathConfig(env: Env): F[Result[OperationOutcome]] = (for {
    from <- env.get[LightSource]("from")
    to   <- env.get[LightSinkName]("to")
  } yield server
    .lightpathConfig(from, to)
    .attempt
    .map(convertResult))
    .getOrElse(Result.failure[OperationOutcome]("Slew parameters could not be parsed.").pure[F])

  def adjustTarget(env: Env): F[Result[OperationOutcome]] = (for {
    target    <- env.get[VirtualTelescope]("target")
    offset    <- env.get[HandsetAdjustment]("offset")
    openLoops <- env.get[Boolean]("openLoops")
  } yield server
    .targetAdjust(target, offset, openLoops)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("Target adjustment parameters could not be parsed.").pure[F]
  )

  def adjustOrigin(env: Env): F[Result[OperationOutcome]] = (for {
    offset    <- env.get[HandsetAdjustment]("offset")
    openLoops <- env.get[Boolean]("openLoops")
  } yield server
    .originAdjust(offset, openLoops)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("Origin adjustment parameters could not be parsed.").pure[F]
  )

  def adjustPointing(env: Env): F[Result[OperationOutcome]] = (for {
    offset <- env.get[HandsetAdjustment]("offset")
  } yield server
    .pointingAdjust(offset)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("Pointing adjustment parameters could not be parsed.").pure[F]
  )

  def resetTargetAdjustment(env: Env): F[Result[OperationOutcome]] = (for {
    target    <- env.get[VirtualTelescope]("target")
    openLoops <- env.get[Boolean]("openLoops")
  } yield server
    .targetOffsetClear(target, openLoops)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("Clear target offset parameters could not be parsed.").pure[F]
  )

  def absorbTargetAdjustment(env: Env): F[Result[OperationOutcome]] = (for {
    target <- env.get[VirtualTelescope]("target")
  } yield server
    .targetOffsetAbsorb(target)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("Absorb target offset parameters could not be parsed.").pure[F]
  )

  def resetOriginAdjustment(env: Env): F[Result[OperationOutcome]] = (for {
    openLoops <- env.get[Boolean]("openLoops")
  } yield server
    .originOffsetClear(openLoops)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("Clear origin offset parameters could not be parsed.").pure[F]
  )

  def acLens(env: Env): F[Result[OperationOutcome]] = (for {
    lens <- env.get[AcLens]("lens")
  } yield server
    .acLens(lens)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("AC lens parameter could not be parsed.").pure[F]
  )

  def acFilter(env: Env): F[Result[OperationOutcome]] = (for {
    filter <- env.get[AcFilter]("filter")
  } yield server
    .acFilter(filter)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("AC filter parameter could not be parsed.").pure[F]
  )

  def acNdFilter(env: Env): F[Result[OperationOutcome]] = (for {
    ndFilter <- env.get[AcNdFilter]("ndFilter")
  } yield server
    .acNdFilter(ndFilter)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("AC ND filter parameter could not be parsed.").pure[F]
  )

  def acWindowSize(env: Env): F[Result[OperationOutcome]] = (for {
    windowSize <- env.get[AcWindow]("size")
  } yield server
    .acWindowSize(windowSize)
    .attempt
    .map(convertResult)).getOrElse(
    Result.failure[OperationOutcome]("AC Window parameter could not be parsed.").pure[F]
  )

  def pwfs1CircularBuffer(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map(
        server
          .pwfs1CircularBuffer(_)
          .attempt
          .map(convertResult)
      )
      .getOrElse(
        Result
          .failure[OperationOutcome]("PWFS1 circular buffer parameter could not be parsed.")
          .pure[F]
      )

  def pwfs2CircularBuffer(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map(
        server
          .pwfs2CircularBuffer(_)
          .attempt
          .map(convertResult)
      )
      .getOrElse(
        Result
          .failure[OperationOutcome]("PWFS2 circular buffer parameter could not be parsed.")
          .pure[F]
      )

  def oiwfsCircularBuffer(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Boolean]("enable")
      .map(
        server
          .oiwfsCircularBuffer(_)
          .attempt
          .map(convertResult)
      )
      .getOrElse(
        Result
          .failure[OperationOutcome]("OIWFS circular buffer parameter could not be parsed.")
          .pure[F]
      )

  def refreshEphemerisFiles(env: Env): F[Result[OperationOutcome]] =
    env
      .get[Option[LocalDate]]("observingNight")
      .map(d => server.refreshEphemerides(d).attempt.map(convertResult))
      .getOrElse(
        Result
          .failure[OperationOutcome]("Ephemeris file refresh parameter could not be parsed.")
          .pure[F]
      )

  def parameterlessCommand(cmd: F[CommandResult]): F[Result[OperationOutcome]] =
    cmd.attempt
      .map(convertResult)

  def convertResult(r: Either[Throwable, CommandResult]): Result[OperationOutcome] = r match {
    case Right(CommandResult.CommandSuccess)      => Result.success(OperationOutcome.success)
    case Right(CommandResult.CommandPaused)       => Result.success(OperationOutcome.success)
    case Right(CommandResult.CommandFailure(msg)) => Result.failure(msg)
    case Left(e)                                  => Result.internalError(e)
  }

  val QueryType: TypeRef        = schema.ref("Query")
  val MutationType: TypeRef     = schema.ref("Mutation")
  val SubscriptionType: TypeRef = schema.ref("Subscription")

  private def selectWfsTarget(name: String, fields: List[(String, Value)]): Elab[Unit] =
    Elab
      .liftR(parseTargetInput(fields).toResult(s"Could not parse ${name}Target parameters."))
      .flatMap(x => Elab.env("target" -> x))

  private def selectProbeTracking(name: String, fields: List[(String, Value)]): Elab[Unit] =
    Elab
      .liftR(
        parseTrackingInput(fields).toResult(s"Could not parse ${name}ProbeTracking parameters.")
      )
      .flatMap(x => Elab.env("config" -> x))

  private def selectWfsObserve(name: String, fields: List[(String, Value)]): Elab[Unit] =
    Elab
      .liftR(parseTimeSpan(fields).toResult(s"Could not parse ${name}Observe parameters."))
      .flatMap(x => Elab.env("period" -> x))

  override val selectElaborator: SelectElaborator = SelectElaborator {
    case (MutationType, "mountFollow", List(Binding("enable", BooleanValue(en))))                 =>
      Elab.env("enable" -> en)
    case (MutationType, "rotatorFollow", List(Binding("enable", BooleanValue(en))))               =>
      Elab.env("enable" -> en)
    case (MutationType, "rotatorConfig", List(Binding("config", ObjectValue(fields))))            =>
      for {
        x <- Elab.liftR(
               parseRotatorConfig(fields).toResult("Could not parse rotatorConfig parameters.")
             )
        _ <- Elab.env("config", x)
      } yield ()
    case (MutationType, "scsFollow", List(Binding("enable", BooleanValue(en))))                   =>
      Elab.env("enable" -> en)
    case (MutationType, "tcsConfig", List(Binding("config", ObjectValue(fields))))                =>
      for {
        x <-
          Elab.liftR(parseTcsConfigInput(fields).toResult("Could not parse TCS config parameters."))
        _ <- Elab.env("config", x)
      } yield ()
    case (MutationType,
          "slew",
          List(Binding("slewOptions", ObjectValue(so)),
               Binding("config", ObjectValue(cf)),
               Binding("obsId", AbsentValue | NullValue)
          )
        ) =>
      for {
        x <-
          Elab.liftR(parseSlewOptionsInput(so).toResult("Could not parse Slew options parameters."))
        _ <- Elab.env("slewOptions" -> x)
        y <- Elab.liftR(parseTcsConfigInput(cf).toResult("Could not parse TCS config parameters."))
        _ <- Elab.env("config" -> y)
        _ <- Elab.env("obsId" -> none[Observation.Id])
      } yield ()
    case (MutationType,
          "slew",
          List(Binding("slewOptions", ObjectValue(so)),
               Binding("config", ObjectValue(cf)),
               Binding("obsId", StringValue(oi))
          )
        ) =>
      for {
        _ <- Elab.env("obsId" -> parseObservationIdInput(oi))
        x <-
          Elab.liftR(parseSlewOptionsInput(so).toResult("Could not parse Slew options parameters."))
        _ <- Elab.env("slewOptions" -> x)
        y <- Elab.liftR(parseTcsConfigInput(cf).toResult("Could not parse TCS config parameters."))
        _ <- Elab.env("config" -> y)
      } yield ()
    case (MutationType, "swapTarget", List(Binding("swapConfig", ObjectValue(fields))))           =>
      for {
        x <-
          Elab.liftR(
            parseSwapConfigInput(fields).toResult("Could not parse swap target parameters.")
          )
        _ <- Elab.env("swapConfig", x)
      } yield ()
    case (MutationType, "restoreTarget", List(Binding("config", ObjectValue(fields))))            =>
      for {
        x <-
          Elab.liftR(
            parseTcsConfigInput(fields).toResult("Could not parse restore target parameters.")
          )
        _ <- Elab.env("config", x)
      } yield ()
    case (MutationType,
          "instrumentSpecifics",
          List(Binding("instrumentSpecificsParams", ObjectValue(fields)))
        ) =>
      for {
        x <- Elab.liftR(
               parseInstrumentSpecificsInput(fields).toResult(
                 "Could not parse instrumentSpecifics parameters."
               )
             )
        _ <- Elab.env("instrumentSpecificsParams" -> x)
      } yield ()
    case (MutationType, "pwfs1Target", List(Binding("target", ObjectValue(fields))))              =>
      selectWfsTarget("pwfs1", fields)
    case (MutationType, "pwfs1ProbeTracking", List(Binding("config", ObjectValue(fields))))       =>
      selectProbeTracking("pwfs1", fields)
    case (MutationType, "pwfs1Follow", List(Binding("enable", BooleanValue(en))))                 =>
      Elab.env("enable" -> en)
    case (MutationType, "pwfs1Observe", List(Binding("period", ObjectValue(fields))))             =>
      selectWfsObserve("pwfs1", fields)
    case (MutationType, "pwfs2Target", List(Binding("target", ObjectValue(fields))))              =>
      selectWfsTarget("pwfs2", fields)
    case (MutationType, "pwfs2ProbeTracking", List(Binding("config", ObjectValue(fields))))       =>
      selectProbeTracking("pwfs2", fields)
    case (MutationType, "pwfs2Follow", List(Binding("enable", BooleanValue(en))))                 =>
      Elab.env("enable" -> en)
    case (MutationType, "pwfs2Observe", List(Binding("period", ObjectValue(fields))))             =>
      selectWfsObserve("pwfs2", fields)
    case (MutationType, "oiwfsTarget", List(Binding("target", ObjectValue(fields))))              =>
      selectWfsTarget("oiwfs", fields)
    case (MutationType, "oiwfsProbeTracking", List(Binding("config", ObjectValue(fields))))       =>
      selectProbeTracking("oiwfs", fields)
    case (MutationType, "oiwfsFollow", List(Binding("enable", BooleanValue(en))))                 =>
      Elab.env("enable" -> en)
    case (MutationType, "oiwfsObserve", List(Binding("period", ObjectValue(fields))))             =>
      selectWfsObserve("oiwfs", fields)
    case (MutationType, "acObserve", List(Binding("period", ObjectValue(fields))))                =>
      for {
        x <- Elab.liftR(
               parseTimeSpan(fields).toResult(
                 "Could not parse acObserve parameters."
               )
             )
        _ <- Elab.env("period" -> x)
      } yield ()
    case (MutationType, "guideEnable", List(Binding("config", ObjectValue(fields))))              =>
      for {
        x <- Elab.liftR(
               parseGuideConfig(fields).toResult(
                 "Could not parse guideEnable parameters."
               )
             )
        _ <- Elab.env("config" -> x)
      } yield ()
    case (MutationType,
          "lightpathConfig",
          List(Binding("from", EnumValue(f)), Binding("to", EnumValue(t)))
        ) =>
      for {
        from <- Elab.liftR(
                  parseEnumerated[LightSource](f).toResult(
                    s"Could not parse lightpathConfig parameter \"from\" \"${f}\""
                  )
                )
        to   <- Elab.liftR(
                  parseEnumerated[LightSinkName](t).toResult(
                    s"Could not parse lightpathConfig parameter \"to\" \"${t}\""
                  )
                )
        _    <- Elab.env("from" -> from)
        _    <- Elab.env("to" -> to)
      } yield ()
    case (MutationType, "acquisitionAdjustment", List(Binding("adjustment", ObjectValue(adj))))   =>
      Elab
        .liftR(
          parseAcquisitionAdjustment(adj)
            .toResult(s"Could not parse adjustment parameter \"adjustment\" \"${adj}\"")
        )
        .flatMap { x =>
          Elab.env("adjustment" -> x)
        }
    case (MutationType,
          "wfsSky",
          List(Binding("wfs", EnumValue(wfs)), Binding("period", ObjectValue(fields)))
        ) =>
      for {
        w <- Elab.liftR(
               parseEnumerated[GuideProbe](wfs).toResult(
                 s"Could not parse wfsSky parameter \"wfs\" ${wfs}"
               )
             )
        t <- Elab.liftR(
               parseTimeSpan(fields).toResult(
                 "Could not parse wfsSky parameter \"period\""
               )
             )
        _ <- Elab.env("wfs" -> w)
        _ <- Elab.env("period" -> t)
      } yield ()
    case (MutationType,
          "adjustTarget",
          List(Binding("target", EnumValue(target)),
               Binding("offset", ObjectValue(offset)),
               Binding("openLoops", BooleanValue(openLoops))
          )
        ) =>
      for {
        t <- Elab.liftR(
               parseEnumerated[VirtualTelescope](target).toResult(
                 s"Could not parse adjustTarget parameter \"target\" \"${target}\""
               )
             )
        o <- Elab.liftR(
               parseHandsetAdjustment(offset).toResult(
                 "Could not parse adjustTarget parameter \"offset\""
               )
             )
        _ <- Elab.env("target", t)
        _ <- Elab.env("offset", o)
        _ <- Elab.env("openLoops", openLoops)
      } yield ()
    case (MutationType,
          "resetTargetAdjustment",
          List(Binding("target", EnumValue(target)), Binding("openLoops", BooleanValue(openLoops)))
        ) =>
      for {
        t <- Elab.liftR(
               parseEnumerated[VirtualTelescope](target).toResult(
                 s"Could not parse resetTargetAdjustment parameter \"target\" \"${target}\""
               )
             )
        _ <- Elab.env("target", t)
        _ <- Elab.env("openLoops", openLoops)
      } yield ()
    case (MutationType, "absorbTargetAdjustment", List(Binding("target", EnumValue(target))))     =>
      for {
        t <- Elab.liftR(
               parseEnumerated[VirtualTelescope](target).toResult(
                 s"Could not parse absorbTargetAdjustment parameter \"target\" \"${target}\""
               )
             )
        _ <- Elab.env("target", t)
      } yield ()
    case (MutationType, "adjustPointing", List(Binding("offset", ObjectValue(offset))))           =>
      for {
        o <- Elab.liftR(
               parseHandsetAdjustment(offset).toResult(
                 "Could not parse adjustPointing parameter \"offset\""
               )
             )
        _ <- Elab.env("offset", o)
      } yield ()
    case (MutationType,
          "adjustOrigin",
          List(Binding("offset", ObjectValue(offset)),
               Binding("openLoops", BooleanValue(openLoops))
          )
        ) =>
      for {
        o <- Elab.liftR(
               parseHandsetAdjustment(offset).toResult(
                 "Could not parse adjustOrigin parameter \"offset\""
               )
             )
        _ <- Elab.env("offset", o)
        _ <- Elab.env("openLoops", openLoops)
      } yield ()
    case (MutationType,
          "resetOriginAdjustment",
          List(Binding("openLoops", BooleanValue(openLoops)))
        ) =>
      Elab.env("openLoops", openLoops)
    case (MutationType, "acLens", List(Binding("lens", EnumValue(name))))                         =>
      for {
        t <- Elab.liftR(
               parseEnumerated[AcLens](name).toResult(
                 s"Could not parse acLens parameter \"lens\" \"${name}\""
               )
             )
        _ <- Elab.env("lens", t)
      } yield ()
    case (MutationType, "acFilter", List(Binding("filter", EnumValue(name))))                     =>
      for {
        t <- Elab.liftR(
               parseEnumerated[AcFilter](name).toResult(
                 s"Could not parse acFilter parameter \"filter\" \"${name}\""
               )
             )
        _ <- Elab.env("filter", t)
      } yield ()
    case (MutationType, "acNdFilter", List(Binding("ndFilter", EnumValue(name))))                 =>
      for {
        t <- Elab.liftR(
               parseEnumerated[AcNdFilter](name).toResult(
                 s"Could not parse acNdFilter parameter \"ndFilter\" \"${name}\""
               )
             )
        _ <- Elab.env("ndFilter", t)
      } yield ()
    case (MutationType, "acWindowSize", List(Binding("size", ObjectValue(l))))                    =>
      for {
        t <- Elab.liftR(
               parseAcWindowSize(l).toResult(
                 "Could not parse acWindowSize parameter \"size\""
               )
             )
        _ <- Elab.env("size", t)
      } yield ()
    case (MutationType, "pwfs1Filter", List(Binding("filter", EnumValue(name))))                  =>
      for {
        t <- Elab.liftR(
               parseEnumerated[PwfsFilter](name).toResult(
                 s"Could not parse pwfs1Filter parameter \"filter\" \"${name}\""
               )
             )
        _ <- Elab.env("filter", t)
      } yield ()
    case (MutationType, "pwfs1FieldStop", List(Binding("fieldStop", EnumValue(name))))            =>
      for {
        t <- Elab.liftR(
               parseEnumerated[PwfsFieldStop](name).toResult(
                 s"Could not parse pwfs1FieldStop parameter \"fieldStop\" \"${name}\""
               )
             )
        _ <- Elab.env("fieldStop", t)
      } yield ()
    case (MutationType, "pwfs2Filter", List(Binding("filter", EnumValue(name))))                  =>
      for {
        t <- Elab.liftR(
               parseEnumerated[PwfsFilter](name).toResult(
                 s"Could not parse pwfs2Filter parameter \"filter\" \"${name}\""
               )
             )
        _ <- Elab.env("filter", t)
      } yield ()
    case (MutationType, "pwfs2FieldStop", List(Binding("fieldStop", EnumValue(name))))            =>
      for {
        t <- Elab.liftR(
               parseEnumerated[PwfsFieldStop](name).toResult(
                 s"Could not parse pwfs2FieldStop parameter \"fieldStop\" \"${name}\""
               )
             )
        _ <- Elab.env("fieldStop", t)
      } yield ()
    case (MutationType, "pwfs1CircularBuffer", List(Binding("enable", BooleanValue(v))))          =>
      Elab.env("enable" -> v)
    case (MutationType, "pwfs2CircularBuffer", List(Binding("enable", BooleanValue(v))))          =>
      Elab.env("enable" -> v)
    case (MutationType, "oiwfsCircularBuffer", List(Binding("enable", BooleanValue(v))))          =>
      Elab.env("enable" -> v)
    case (MutationType,
          "refreshEphemerisFiles",
          List(Binding("observingNight", AbsentValue | NullValue))
        ) =>
      Elab.env("observingNight", none[LocalDate])
    case (MutationType, "refreshEphemerisFiles", List(Binding("observingNight", StringValue(l)))) =>
      Elab
        .liftR(
          parseDate(l)
            .map(_.some)
            .toResult(s"Could not parse refreshEphemerisFiles parameter \"observingNight\" ${l}")
        )
        .flatMap(v => Elab.env("observingNight", v))
    case (QueryType, "instrumentPort", List(Binding("instrument", EnumValue(ins))))               =>
      Elab
        .liftR(
          parseEnumerated[Instrument](ins)
            .toResult(s"Could not parse instrumentPort parameter \"instrument\" \"${ins}\"")
        )
        .flatMap(x => Elab.env("instrument" -> x))
  }

  override val typeMappings: TypeMappings = TypeMappings(
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          RootEffect.computeEncodable("telescopeState")((_, _) => telescopeState),
          RootEffect.computeEncodable("guideState")((_, _) => guideState),
          RootEffect.computeEncodable("guidersQualityValues")((_, _) => guidersQualityValues),
          RootEffect.computeEncodable("navigateState")((_, _) => navigateState),
          RootEffect.computeEncodable("instrumentPort")((_, env) => instrumentPort(env)),
          RootEffect.computeEncodable("serverVersion")((_, _) => serverVersion),
          RootEffect.computeEncodable("targetAdjustmentOffsets")((_, _) => targetAdjustmentOffsets),
          RootEffect.computeEncodable("originAdjustmentOffset")((_, _) => originAdjustmentOffset),
          RootEffect.computeEncodable("pointingAdjustmentOffset")((_, _) =>
            pointingAdjustmentOffset
          ),
          RootEffect.computeEncodable("serverConfiguration")((_, _) => serverConfig),
          RootEffect.computeEncodable("acMechsState")((_, _) => acMechsState),
          RootEffect.computeEncodable("pwfs1MechsState")((_, _) => pwfs1MechsState),
          RootEffect.computeEncodable("pwfs2MechsState")((_, _) => pwfs2MechsState),
          RootEffect.computeEncodable("bafflesState")((_, _) => bafflesState),
          RootEffect.computeEncodable("pwfs1ConfigState")((_, _) => pwfs1ConfigState),
          RootEffect.computeEncodable("pwfs2ConfigState")((_, _) => pwfs2ConfigState),
          RootEffect.computeEncodable("oiwfsConfigState")((_, _) => oiwfsConfigState)
        )
      ),
      ObjectMapping(
        tpe = MutationType,
        fieldMappings = List(
          RootEffect.computeEncodable("mountPark")((_, _) => parameterlessCommand(server.mcsPark)),
          RootEffect.computeEncodable("mountFollow")((_, env) => mountFollow(env)),
          RootEffect.computeEncodable("rotatorPark")((_, _) =>
            parameterlessCommand(server.rotPark)
          ),
          RootEffect.computeEncodable("rotatorFollow")((_, env) => rotatorFollow(env)),
          RootEffect.computeEncodable("rotatorConfig")((_, env) => rotatorConfig(env)),
          RootEffect.computeEncodable("scsFollow")((_, env) => scsFollow(env)),
          RootEffect.computeEncodable("tcsConfig")((_, env) => tcsConfig(env)),
          RootEffect.computeEncodable("slew")((_, env) => slew(env)),
          RootEffect.computeEncodable("swapTarget")((_, env) => swapTarget(env)),
          RootEffect.computeEncodable("restoreTarget")((_, env) => restoreTarget(env)),
          RootEffect.computeEncodable("instrumentSpecifics")((_, env) => instrumentSpecifics(env)),
          RootEffect.computeEncodable("pwfs1Target")((_, env) =>
            wfsTarget("pwfs1", server.pwfs1Target)(env)
          ),
          RootEffect.computeEncodable("pwfs1ProbeTracking")((_, env) =>
            wfsProbeTracking("pwfs1", server.pwfs1ProbeTracking)(env)
          ),
          RootEffect.computeEncodable("pwfs1Park")((_, _) =>
            parameterlessCommand(server.pwfs1Park)
          ),
          RootEffect.computeEncodable("pwfs1Follow")((_, env) =>
            wfsFollow("pwfs1", server.pwfs1Follow)(env)
          ),
          RootEffect.computeEncodable("pwfs1Observe")((_, env) =>
            wfsObserve("pwfs1", server.pwfs1Observe)(env)
          ),
          RootEffect.computeEncodable("pwfs1StopObserve")((_, _) =>
            parameterlessCommand(server.pwfs1StopObserve)
          ),
          RootEffect.computeEncodable("pwfs1Filter")((_, env) =>
            wfsFilter("pwfs1", server.pwfs1Filter)(env)
          ),
          RootEffect.computeEncodable("pwfs1FieldStop")((_, env) =>
            wfsFieldStop("pwfs1", server.pwfs1FieldStop)(env)
          ),
          RootEffect.computeEncodable("pwfs2Target")((_, env) =>
            wfsTarget("pwfs2", server.pwfs2Target)(env)
          ),
          RootEffect.computeEncodable("pwfs2ProbeTracking")((_, env) =>
            wfsProbeTracking("pwfs2", server.pwfs2ProbeTracking)(env)
          ),
          RootEffect.computeEncodable("pwfs2Park")((_, _) =>
            parameterlessCommand(server.pwfs2Park)
          ),
          RootEffect.computeEncodable("pwfs2Follow")((_, env) =>
            wfsFollow("pwfs2", server.pwfs2Follow)(env)
          ),
          RootEffect.computeEncodable("pwfs2Observe")((_, env) =>
            wfsObserve("pwfs2", server.pwfs2Observe)(env)
          ),
          RootEffect.computeEncodable("pwfs2StopObserve")((_, _) =>
            parameterlessCommand(server.pwfs2StopObserve)
          ),
          RootEffect.computeEncodable("pwfs2Filter")((_, env) =>
            wfsFilter("pwfs2", server.pwfs2Filter)(env)
          ),
          RootEffect.computeEncodable("pwfs2FieldStop")((_, env) =>
            wfsFieldStop("pwfs2", server.pwfs2FieldStop)(env)
          ),
          RootEffect.computeEncodable("oiwfsTarget")((_, env) =>
            wfsTarget("oiwfs", server.oiwfsTarget)(env)
          ),
          RootEffect.computeEncodable("oiwfsProbeTracking")((_, env) =>
            wfsProbeTracking("oiwfs", server.oiwfsProbeTracking)(env)
          ),
          RootEffect.computeEncodable("oiwfsPark")((_, _) =>
            parameterlessCommand(server.oiwfsPark)
          ),
          RootEffect.computeEncodable("oiwfsFollow")((_, env) =>
            wfsFollow("oiwfs", server.oiwfsFollow)(env)
          ),
          RootEffect.computeEncodable("oiwfsObserve")((_, env) =>
            wfsObserve("oiwfs", server.oiwfsObserve)(env)
          ),
          RootEffect.computeEncodable("oiwfsStopObserve")((_, _) =>
            parameterlessCommand(server.oiwfsStopObserve)
          ),
          RootEffect.computeEncodable("acObserve")((_, env) => acObserve(env)),
          RootEffect.computeEncodable("acStopObserve")((_, _) =>
            parameterlessCommand(server.acStopObserve)
          ),
          RootEffect.computeEncodable("guideEnable")((_, env) => guideEnable(env)),
          RootEffect.computeEncodable("guideDisable")((_, _) =>
            parameterlessCommand(server.disableGuide)
          ),
          RootEffect.computeEncodable("m1Park")((_, _) => parameterlessCommand(server.m1Park)),
          RootEffect.computeEncodable("m1Unpark")((_, _) => parameterlessCommand(server.m1Unpark)),
          RootEffect.computeEncodable("m1OpenLoopOff")((_, _) =>
            parameterlessCommand(server.m1OpenLoopOff)
          ),
          RootEffect.computeEncodable("m1OpenLoopOn")((_, _) =>
            parameterlessCommand(server.m1OpenLoopOn)
          ),
          RootEffect.computeEncodable("m1ZeroFigure")((_, _) =>
            parameterlessCommand(server.m1ZeroFigure)
          ),
          RootEffect.computeEncodable("m1LoadAoFigure")((_, _) =>
            parameterlessCommand(server.m1LoadAoFigure)
          ),
          RootEffect.computeEncodable("m1LoadNonAoFigure")((_, _) =>
            parameterlessCommand(server.m1LoadNonAoFigure)
          ),
          RootEffect.computeEncodable("lightpathConfig")((_, env) => lightpathConfig(env)),
          RootEffect.computeEncodable("acquisitionAdjustment") { (_, env) =>
            acquisitionAdjustment(env)
          },
          RootEffect.computeEncodable("wfsSky") { (_, env) =>
            wfsSky(env)
          },
          RootEffect.computeEncodable("adjustTarget") { (_, env) =>
            adjustTarget(env)
          },
          RootEffect.computeEncodable("adjustPointing") { (_, env) =>
            adjustPointing(env)
          },
          RootEffect.computeEncodable("adjustOrigin") { (_, env) =>
            adjustOrigin(env)
          },
          RootEffect.computeEncodable("resetTargetAdjustment")((_, env) =>
            resetTargetAdjustment(env)
          ),
          RootEffect.computeEncodable("absorbTargetAdjustment")((_, env) =>
            absorbTargetAdjustment(env)
          ),
          RootEffect.computeEncodable("resetLocalPointingAdjustment")((_, _) =>
            parameterlessCommand(server.pointingOffsetClearLocal)
          ),
          RootEffect.computeEncodable("resetGuidePointingAdjustment")((_, _) =>
            parameterlessCommand(server.pointingOffsetClearGuide)
          ),
          RootEffect.computeEncodable("absorbGuidePointingAdjustment")((_, _) =>
            parameterlessCommand(server.pointingOffsetAbsorbGuide)
          ),
          RootEffect.computeEncodable("resetOriginAdjustment")((_, env) =>
            resetOriginAdjustment(env)
          ),
          RootEffect.computeEncodable("absorbOriginAdjustment")((_, _) =>
            parameterlessCommand(server.originOffsetAbsorb)
          ),
          RootEffect.computeEncodable("acLens")((_, env) => acLens(env)),
          RootEffect.computeEncodable("acFilter")((_, env) => acFilter(env)),
          RootEffect.computeEncodable("acNdFilter")((_, env) => acNdFilter(env)),
          RootEffect.computeEncodable("acWindowSize")((_, env) => acWindowSize(env)),
          RootEffect.computeEncodable("pwfs1CircularBuffer")((_, env) => pwfs1CircularBuffer(env)),
          RootEffect.computeEncodable("pwfs2CircularBuffer")((_, env) => pwfs2CircularBuffer(env)),
          RootEffect.computeEncodable("oiwfsCircularBuffer")((_, env) => oiwfsCircularBuffer(env)),
          RootEffect.computeEncodable("refreshEphemerisFiles")((_, env) =>
            refreshEphemerisFiles(env)
          )
        )
      ),
      ObjectMapping(
        tpe = SubscriptionType,
        List(
          RootStream.computeCursor("logMessage") { (p, env) =>
            (Stream.evalSeq(logBuffer.get) ++
              logTopic.subscribe(1024))
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("guideState") { (p, env) =>
            guideStateTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("guidersQualityValues") { (p, env) =>
            guidersQualityTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("telescopeState") { (p, env) =>
            telescopeStateTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("navigateState") { (p, env) =>
            server.getNavigateStateStream
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("acquisitionAdjustmentState") { (p, env) =>
            acquisitionAdjustmentTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("targetAdjustmentOffsets") { (p, env) =>
            targetAdjustmentTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("originAdjustmentOffset") { (p, env) =>
            originAdjustmentTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("pointingAdjustmentOffset") { (p, env) =>
            pointingAdjustmentTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("acMechsState") { (p, env) =>
            acMechsTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("pwfs1MechsState") { (p, env) =>
            pwfs1MechsTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("pwfs2MechsState") { (p, env) =>
            pwfs2MechsTopic
              .subscribe(1024)
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("pwfs1ConfigState") { (p, env) =>
            Stream
              .eval(server.getPwfs1ConfigurationStream)
              .flatten
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("pwfs2ConfigState") { (p, env) =>
            Stream
              .eval(server.getPwfs2ConfigurationStream)
              .flatten
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          },
          RootStream.computeCursor("oiwfsConfigState") { (p, env) =>
            Stream
              .eval(server.getOiwfsConfigurationStream)
              .flatten
              .map(_.asJson)
              .map(circeCursor(p, env, _))
              .map(Result.success)
          }
        )
      )
    )
  )
}

object NavigateMappings extends GrackleParsers {

  def loadSchema[F[_]: Sync]: F[Result[Schema]] = SchemaStitcher
    .apply[F](JPath.of("navigate.graphql"), SourceResolver.fromResource(getClass.getClassLoader))
    .build

  def apply[F[_]: Sync](
    config:                     NavigateConfiguration,
    server:                     NavigateEngine[F],
    logTopic:                   Topic[F, ILoggingEvent],
    guideStateTopic:            Topic[F, GuideState],
    guidersQualityTopic:        Topic[F, GuidersQualityValues],
    telescopeStateTopic:        Topic[F, TelescopeState],
    acquisitionAdjustmentTopic: Topic[F, AcquisitionAdjustment],
    targetAdjustmentTopic:      Topic[F, TargetOffsets],
    originAdjustmentTopic:      Topic[F, FocalPlaneOffset],
    pointingAdjustmentTopic:    Topic[F, PointingCorrections],
    acMechsTopic:               Topic[F, AcMechsState],
    pwfs1MechsTopic:            Topic[F, PwfsMechsState],
    pwfs2MechsTopic:            Topic[F, PwfsMechsState],
    logBuffer:                  Ref[F, Seq[ILoggingEvent]]
  ): F[NavigateMappings[F]] = loadSchema.flatMap {
    case Result.Success(schema)           =>
      new NavigateMappings[F](
        config,
        server,
        logTopic,
        guideStateTopic,
        guidersQualityTopic,
        telescopeStateTopic,
        acquisitionAdjustmentTopic,
        targetAdjustmentTopic,
        originAdjustmentTopic,
        pointingAdjustmentTopic,
        acMechsTopic,
        pwfs1MechsTopic,
        pwfs2MechsTopic,
        logBuffer
      )(schema)
        .pure[F]
    case Result.Warning(problems, schema) =>
      new NavigateMappings[F](
        config,
        server,
        logTopic,
        guideStateTopic,
        guidersQualityTopic,
        telescopeStateTopic,
        acquisitionAdjustmentTopic,
        targetAdjustmentTopic,
        originAdjustmentTopic,
        pointingAdjustmentTopic,
        acMechsTopic,
        pwfs1MechsTopic,
        pwfs2MechsTopic,
        logBuffer
      )(schema)
        .pure[F]
    case Result.Failure(problems)         =>
      Sync[F].raiseError[NavigateMappings[F]](
        new Throwable(
          s"Unable to load schema because: ${problems.map(_.message).toList.mkString(",")}"
        )
      )
    case Result.InternalError(error)      =>
      Sync[F].raiseError[NavigateMappings[F]](
        new Throwable(s"Unable to load schema because: ${error.getMessage}")
      )

  }

  def parseObservationIdInput(oi: String): Option[Observation.Id] =
    parseGid[Observation.Id](oi, "Observation Id").toOption

  def parseSlewOptionsInput(l: List[(String, Value)]): Option[SlewOptions] = for {
    zct  <-
      l.collectFirst { case ("zeroChopThrow", BooleanValue(v)) => v }.map(ZeroChopThrow(_))
    zso  <- l.collectFirst { case ("zeroSourceOffset", BooleanValue(v)) => v }
              .map(ZeroSourceOffset(_))
    zsdt <- l.collectFirst { case ("zeroSourceDiffTrack", BooleanValue(v)) => v }
              .map(ZeroSourceDiffTrack(_))
    zmo  <- l.collectFirst { case ("zeroMountOffset", BooleanValue(v)) => v }
              .map(ZeroMountOffset(_))
    zmdt <- l.collectFirst { case ("zeroMountDiffTrack", BooleanValue(v)) => v }
              .map(ZeroMountDiffTrack(_))
    stf  <- l.collectFirst { case ("shortcircuitTargetFilter", BooleanValue(v)) => v }
              .map(ShortcircuitTargetFilter(_))
    smf  <- l.collectFirst { case ("shortcircuitMountFilter", BooleanValue(v)) => v }
              .map(ShortcircuitMountFilter(_))
    rp   <-
      l.collectFirst { case ("resetPointing", BooleanValue(v)) => v }.map(ResetPointing(_))
    sg   <- l.collectFirst { case ("stopGuide", BooleanValue(v)) => v }.map(StopGuide(_))
    zgo  <- l.collectFirst { case ("zeroGuideOffset", BooleanValue(v)) => v }
              .map(ZeroGuideOffset(_))
    zio  <- l.collectFirst { case ("zeroInstrumentOffset", BooleanValue(v)) => v }
              .map(ZeroInstrumentOffset(_))
    ap1  <-
      l.collectFirst { case ("autoparkPwfs1", BooleanValue(v)) => v }.map(AutoparkPwfs1(_))
    ap2  <-
      l.collectFirst { case ("autoparkPwfs2", BooleanValue(v)) => v }.map(AutoparkPwfs2(_))
    ao   <-
      l.collectFirst { case ("autoparkOiwfs", BooleanValue(v)) => v }.map(AutoparkOiwfs(_))
    ag   <- l.collectFirst { case ("autoparkGems", BooleanValue(v)) => v }.map(AutoparkGems(_))
    aa   <-
      l.collectFirst { case ("autoparkAowfs", BooleanValue(v)) => v }.map(AutoparkAowfs(_))
  } yield SlewOptions(zct, zso, zsdt, zmo, zmdt, stf, smf, rp, sg, zgo, zio, ap1, ap2, ao, ag, aa)

  def parseSiderealTarget(
    name:         String,
    centralWavel: Option[Wavelength],
    l:            List[(String, Value)]
  ): Option[Target.SiderealTarget] = for {
    ra    <- l.collectFirst { case ("ra", ObjectValue(v)) => parseRightAscension(v) }.flatten
    dec   <- l.collectFirst { case ("dec", ObjectValue(v)) => parseDeclination(v) }.flatten
    epoch <- l.collectFirst { case ("epoch", StringValue(v)) => parseEpoch(v) }.flatten
  } yield Target.SiderealTarget(
    name,
    centralWavel,
    Coordinates(ra, dec),
    epoch,
    l.collectFirst { case ("properMotion", ObjectValue(v)) => parseProperMotion(v) }.flatten,
    l.collectFirst { case ("radialVelocity", ObjectValue(v)) => parseRadialVelocity(v) }.flatten,
    l.collectFirst { case ("parallax", ObjectValue(v)) => parseParallax(v) }.flatten
  )

  def parseNonSiderealTarget(
    name: String,
    w:    Option[Wavelength],
    l:    List[(String, Value)]
  ): Option[Target.EphemerisTarget] = (for {
    keyType <- l.collectFirst { case ("keyType", EnumValue(v)) =>
                 parseEnumerated[EphemerisKeyType](v)
               }.flatten
    des     <- l.collectFirst { case ("des", StringValue(v)) => v }
    key     <- Ephemeris.Key.fromTypeAndDes.getOption(keyType, des)
  } yield key)
    .orElse(l.collectFirst { case ("key", StringValue(v)) =>
      Ephemeris.Key.fromString.getOption(v)
    }.flatten)
    .map(k => Target.EphemerisTarget(name, w, k))

  def parseAzElTarget(
    name: String,
    w:    Option[Wavelength],
    l:    List[(String, Value)]
  ): Option[Target.AzElTarget] = for {
    az <- l.collectFirst { case ("azimuth", ObjectValue(v)) => parseAngle(v) }.flatten
    el <- l.collectFirst { case ("elevation", ObjectValue(v)) => parseAngle(v) }.flatten
  } yield Target.AzElTarget(
    name,
    w,
    Target.AzElCoordinates(
      Target.Azimuth(az),
      Target.Elevation(el)
    )
  )

  def parseEphemerisTarget(
    @annotation.unused name: String,
    @annotation.unused w:    Option[Wavelength],
    @annotation.unused l:    List[(String, Value)]
  ): Option[Target.EphemerisTarget] = none

  def parseTargetInput(l: List[(String, Value)]): Option[Target] = for {
    nm <- l.collectFirst { case ("name", StringValue(v)) => v }
    wv <- l.collectFirst { case ("wavelength", ObjectValue(v)) => parseWavelength(v) } match {
            case Some(None) => None
            case None       => Some(None)
            case x          => x
          }
    bt <- l.collectFirst { case ("sidereal", ObjectValue(v)) => v }
            .flatMap[Target](parseSiderealTarget(nm, wv, _))
            .orElse(
              l.collectFirst { case ("nonsidereal", ObjectValue(v)) => v }
                .flatMap(parseNonSiderealTarget(nm, wv, _))
            )
            .orElse(
              l.collectFirst { case ("azel", ObjectValue(v)) => v }
                .flatMap(parseAzElTarget(nm, wv, _))
            )
  } yield bt

  def parseGid[A: Gid](s: String, name: String): Either[String, A] =
    Gid[A].fromString.getOption(s).toRight(s"'$s' is not a valid $name id")

  def parseGuideTargetInput(l: List[(String, Value)]): Option[Target] = for {
    nm <- l.collectFirst { case ("name", StringValue(v)) => v }
    bt <- l.collectFirst { case ("sidereal", ObjectValue(v)) => v }
            .flatMap[Target](parseSiderealTarget(nm, None, _))
            .orElse(
              l.collectFirst { case ("nonsidereal", ObjectValue(v)) => v }
                .flatMap(parseNonSiderealTarget(nm, None, _))
            )
  } yield bt

  def parseTrackingInput(l: List[(String, Value)]): Option[TrackingConfig] = for {
    aa <- l.collectFirst { case ("nodAchopA", BooleanValue(v)) => v }
    ab <- l.collectFirst { case ("nodAchopB", BooleanValue(v)) => v }
    ba <- l.collectFirst { case ("nodBchopA", BooleanValue(v)) => v }
    bb <- l.collectFirst { case ("nodBchopB", BooleanValue(v)) => v }
  } yield TrackingConfig(aa, ab, ba, bb)

  def parseOrigin(l: List[(String, Value)]): Option[Origin] = for {
    x <- l.collectFirst { case ("x", ObjectValue(v)) => parseAngle(v) }.flatten
    y <- l.collectFirst { case ("y", ObjectValue(v)) => parseAngle(v) }.flatten
  } yield Origin(x, y)

  def parseInstrumentSpecificsInput(l: List[(String, Value)]): Option[InstrumentSpecifics] = for {
    iaa       <- l.collectFirst { case ("iaa", ObjectValue(v)) => parseAngle(v) }.flatten
    focOffset <- l.collectFirst { case ("focusOffset", ObjectValue(v)) => parseDistance(v) }.flatten
    agName    <- l.collectFirst { case ("agName", StringValue(v)) => v }
    origin    <- l.collectFirst { case ("origin", ObjectValue(v)) => parseOrigin(v) }.flatten
  } yield InstrumentSpecifics(iaa, focOffset, agName, origin)

  def parseGuiderConfig(l: List[(String, Value)]): Option[GuiderConfig] = for {
    target   <- l.collectFirst { case ("target", ObjectValue(v)) => parseGuideTargetInput(v) }.flatten
    tracking <- l.collectFirst { case ("tracking", ObjectValue(v)) =>
                  parseTrackingInput(v)
                }.flatten
  } yield GuiderConfig(target, tracking)

  def parseRotatorConfig(l: List[(String, Value)]): Option[RotatorTrackConfig] = for {
    ipa  <- l.collectFirst { case ("ipa", ObjectValue(v)) => parseAngle(v) }.flatten
    mode <- l.collectFirst { case ("mode", EnumValue(v)) =>
              parseEnumerated[RotatorTrackingMode](v)
            }.flatten
  } yield RotatorTrackConfig(ipa, mode)

  def parseBafflesAuto(l: List[(String, Value)]): Option[BafflesConfig] = for {
    vis <- l.collectFirst { case ("visibleLimit", ObjectValue(v)) => parseWavelength(v) }.flatten
    ni  <- l.collectFirst { case ("nearirLimit", ObjectValue(v)) => parseWavelength(v) }.flatten
  } yield BafflesConfig.AutoConfig(vis, ni)

  def parseBafflesManual(l: List[(String, Value)]): Option[BafflesConfig] = for {
    ctr <- l.collectFirst { case ("centralBaffle", EnumValue(v)) =>
             parseEnumerated[CentralBafflePosition](v)
           }.flatten
    dep <- l.collectFirst { case ("deployableBaffle", EnumValue(v)) =>
             parseEnumerated[DeployableBafflePosition](v)
           }.flatten
  } yield BafflesConfig.ManualConfig(ctr, dep)

  def parseBaffles(l: List[(String, Value)]): Option[BafflesConfig] = l
    .collectFirst { case ("autoConfig", ObjectValue(v)) => parseBafflesAuto(v) }
    .flatten
    .orElse(l.collectFirst { case ("manualConfig", ObjectValue(v)) =>
      parseBafflesManual(v)
    }.flatten)

  def parseTcsConfigInput(l: List[(String, Value)]): Option[TcsConfig] = for {
    t   <- l.collectFirst { case ("sourceATarget", ObjectValue(v)) => parseTargetInput(v) }.flatten
    inp <- l.collectFirst { case ("instParams", ObjectValue(v)) =>
             parseInstrumentSpecificsInput(v)
           }.flatten
    p1  <-
      l.collectFirst { case ("pwfs1", ObjectValue(v)) => parseGuiderConfig(v) } match {
        case Some(None) => None
        case None       => Some(None)
        case x          => x
      }
    p2  <-
      l.collectFirst { case ("pwfs2", ObjectValue(v)) => parseGuiderConfig(v) } match {
        case Some(None) => None
        case None       => Some(None)
        case x          => x
      }
    oi  <-
      l.collectFirst { case ("oiwfs", ObjectValue(v)) => parseGuiderConfig(v) } match {
        case Some(None) => None
        case None       => Some(None)
        case x          => x
      }
    rc  <- l.collectFirst { case ("rotator", ObjectValue(v)) => parseRotatorConfig(v) }.flatten
    ins <- l.collectFirst { case ("instrument", EnumValue(v)) =>
             parseEnumerated[Instrument](v)
           }.flatten
    bfs <- l.collectFirst { case ("baffles", ObjectValue(v)) => v } match {
             case Some(x) =>
               if (x.forall(_._2 == Value.AbsentValue)) Some(None) else parseBaffles(x).map(_.some)
             case None    => Some(None)
           }
  } yield TcsConfig(t, inp, p1, p2, oi, rc, ins, bfs)

  def parseSwapConfigInput(l: List[(String, Value)]): Option[SwapConfig] = for {
    t   <- l.collectFirst { case ("guideTarget", ObjectValue(v)) => parseTargetInput(v) }.flatten
    inp <- l.collectFirst { case ("acParams", ObjectValue(v)) =>
             parseInstrumentSpecificsInput(v)
           }.flatten
    rc  <- l.collectFirst { case ("rotator", ObjectValue(v)) => parseRotatorConfig(v) }.flatten
  } yield SwapConfig(t, inp, rc)

  def parseProbeGuide(l: List[(String, Value)]): Option[ProbeGuide] = for {
    f <- l.collectFirst { case ("from", EnumValue(v)) => parseEnumerated[GuideProbe](v) }.flatten
    t <- l.collectFirst { case ("to", EnumValue(v)) => parseEnumerated[GuideProbe](v) }.flatten
  } yield ProbeGuide(f, t)

  def parseGuideConfig(l: List[(String, Value)]): Option[TelescopeGuideConfig] = {
    val m2: List[TipTiltSource] = l.collectFirst { case ("m2Inputs", ListValue(v)) =>
      v.collect { case EnumValue(v) => parseEnumerated[TipTiltSource](v) }.flattenOption
    }.orEmpty

    val m1: Option[M1Source] = l.collectFirst { case ("m1Input", EnumValue(v)) =>
      parseEnumerated[M1Source](v)
    }.flatten

    val coma = l.collectFirst { case ("m2Coma", BooleanValue(v)) => v }.exists(identity)

    val dayTimeMode = l.collectFirst { case ("daytimeMode", BooleanValue(v)) => v }.exists(identity)

    val probeGuide =
      l.collectFirst { case ("probeGuide", ObjectValue(v)) => parseProbeGuide(v) }.flatten

    l.collectFirst { case ("mountOffload", BooleanValue(v)) => v }
      .map { mount =>
        TelescopeGuideConfig(
          MountGuideOption(mount),
          m1.map(M1GuideConfig.M1GuideOn(_)).getOrElse(M1GuideConfig.M1GuideOff),
          m2.isEmpty.fold(
            M2GuideConfig.M2GuideOff,
            M2GuideConfig.M2GuideOn(ComaOption(coma && m1.isDefined), m2.toSet)
          ),
          Some(dayTimeMode),
          probeGuide
        )
      }
  }

  def parseOffset(l: List[(String, Value)]): Option[Offset] =
    for {
      p <- l.collectFirst { case ("p", ObjectValue(v)) => parseAngle(v) }.flatten
      q <- l.collectFirst { case ("q", ObjectValue(v)) => parseAngle(v) }.flatten
    } yield Offset(p.p, q.q)

  def parseAcquisitionAdjustment(l: List[(String, Value)]): Option[AcquisitionAdjustment] =
    for {
      o  <-
        l.collectFirst { case ("offset", ObjectValue(v)) => parseOffset(v) }.flatten
      ipa = l.collectFirst { case ("ipa", ObjectValue(v)) => parseAngle(v) }.flatten
      iaa = l.collectFirst { case ("iaa", ObjectValue(v)) => parseAngle(v) }.flatten
      cmd = l.collectFirst { case ("command", Value.EnumValue(v)) =>
              parseEnumerated[AcquisitionAdjustmentCommand](v)
            }.flatten
    } yield cmd.fold(AcquisitionAdjustment(o, ipa, iaa))(AcquisitionAdjustment(o, ipa, iaa, _))

  def parseHandsetAdjustment(l: List[(String, Value)]): Option[HandsetAdjustment] =
    l.find(_._2 != Value.AbsentValue) match {
      case Some(("horizontalAdjustment", ObjectValue(n))) =>
        for {
          daz <- n.collectFirst { case ("azimuth", ObjectValue(m)) => parseAngle(m) }.flatten
          del <- n.collectFirst { case ("elevation", ObjectValue(m)) => parseAngle(m) }.flatten
        } yield HandsetAdjustment.HorizontalAdjustment(daz, del)
      case Some(("focalPlaneAdjustment", ObjectValue(n))) =>
        for {
          dx <- n.collectFirst { case ("deltaX", ObjectValue(m)) => parseAngle(m) }.flatten
          dy <- n.collectFirst { case ("deltaY", ObjectValue(m)) => parseAngle(m) }.flatten
        } yield HandsetAdjustment.FocalPlaneAdjustment(FocalPlaneOffset(DeltaX(dx), DeltaY(dy)))
      case Some(("instrumentAdjustment", ObjectValue(n))) =>
        parseOffset(n).map(HandsetAdjustment.InstrumentAdjustment.apply)
      case Some(("equatorialAdjustment", ObjectValue(n))) =>
        for {
          dra  <- n.collectFirst { case ("deltaRA", ObjectValue(m)) => parseAngle(m) }.flatten
          ddec <- n.collectFirst { case ("deltaDec", ObjectValue(m)) => parseAngle(m) }.flatten
        } yield HandsetAdjustment.EquatorialAdjustment(dra, ddec)
      case Some(("probeFrameAdjustment", ObjectValue(n))) =>
        for {
          probe <- n.collectFirst { case ("probeFrame", EnumValue(name)) =>
                     parseEnumerated[GuideProbe](name)
                   }.flatten
          du    <- n.collectFirst { case ("deltaU", ObjectValue(m)) => parseAngle(m) }.flatten
          dv    <- n.collectFirst { case ("deltaV", ObjectValue(m)) => parseAngle(m) }.flatten
          aa    <- n.collectFirst { case ("alignAngle", ObjectValue(m)) => m } match {
                     case None    => Some(None)
                     case Some(x) => parseAngle(x).map(_.some)
                   }
        } yield HandsetAdjustment.ProbeFrameAdjustment(probe, du, dv, aa)
      case _                                              => none
    }

  def parseAcWindowSize(l: List[(String, Value)]): Option[AcWindow] =
    l.collectFirst { case ("type", EnumValue(v)) => v }
      .flatMap {
        case "FULL"           => AcWindow.Full.some
        case "WINDOW_200X200" =>
          l.collectFirst { case ("center", ObjectValue(l)) => parseAcWindowCenter(l) }.flatten.map {
            (x, y) => AcWindow.Square200(x, y)
          }
        case "WINDOW_100X100" =>
          l.collectFirst { case ("center", ObjectValue(l)) => parseAcWindowCenter(l) }.flatten.map {
            (x, y) => AcWindow.Square100(x, y)
          }
        case _                => none
      }

  def parseAcWindowCenter(l: List[(String, Value)]): Option[(Int, Int)] = for {
    x <- l.collectFirst { case ("x", IntValue(v)) => v }
    y <- l.collectFirst { case ("y", IntValue(v)) => v }
  } yield (x, y)

  def parseDate(s: String): Option[LocalDate] = Validated.catchNonFatal(LocalDate.parse(s)).toOption

}
