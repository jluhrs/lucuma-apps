// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s

import cats.*
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.spi.IThrowableProxy
import ch.qos.logback.classic.spi.LoggerContextVO
import fs2.Stream
import fs2.concurrent.SignallingRef
import fs2.concurrent.Topic
import io.circe.Decoder
import io.circe.Decoder.Result
import io.circe.DecodingFailure
import io.circe.Json
import lucuma.core.enums.ComaOption
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.M1Source
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.Site
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.GuideConfig
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.Observation
import lucuma.core.model.ProbeGuide
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.horizons.HorizonsClient
import monocle.Focus.focus
import munit.CatsEffectSuite
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.AcquisitionAdjustment
import navigate.model.AllWfsConfiguration
import navigate.model.BafflesState
import navigate.model.CommandResult
import navigate.model.FocalPlaneOffset
import navigate.model.HandsetAdjustment
import navigate.model.HandsetAdjustment.HorizontalAdjustment
import navigate.model.InstrumentSpecifics
import navigate.model.NavigateEvent
import navigate.model.NavigateState
import navigate.model.PointingCorrections
import navigate.model.PwfsMechsState
import navigate.model.RotatorAngle
import navigate.model.RotatorTrackConfig
import navigate.model.ServerConfiguration
import navigate.model.SlewOptions
import navigate.model.SwapConfig
import navigate.model.Target
import navigate.model.TcsConfig
import navigate.model.TrackingConfig
import navigate.model.WfsConfiguration
import navigate.model.config.NavigateConfiguration
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.AcquisitionAdjustmentCommand
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.DomeMode
import navigate.model.enums.LightSource
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server.NavigateEngine
import navigate.server.OdbProxy
import navigate.server.Systems
import navigate.server.tcs.FollowStatus
import navigate.server.tcs.FollowStatus.*
import navigate.server.tcs.GuideState
import navigate.server.tcs.GuidersQualityValues
import navigate.server.tcs.MechSystemState
import navigate.server.tcs.ParkStatus
import navigate.server.tcs.ParkStatus.*
import navigate.server.tcs.TargetOffsets
import navigate.server.tcs.TcsNorthController
import navigate.server.tcs.TcsNorthControllerSim
import navigate.server.tcs.TcsSouthController
import navigate.server.tcs.TcsSouthControllerSim
import navigate.server.tcs.TelescopeState
import navigate.web.server.OcsBuildInfo
import org.http4s.HttpApp
import org.http4s.Uri
import org.http4s.client.Client
import org.slf4j.Marker
import org.slf4j.event.KeyValuePair
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import java.time.LocalDate
import java.util
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.given

class NavigateMappingsSuite extends CatsEffectSuite {
  import NavigateMappingsTest.*
  import NavigateMappingsTest.given

  def extractResult[T: Decoder](j: Json, mutation: String): Option[T] = j.hcursor
    .downField("data")
    .downField(mutation)
    .as[T]
    .toOption

  test("Process mount follow command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { mountFollow(enable: true) { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, "mountFollow").exists(_ === OperationOutcome.success)
    )

  }

  test("Process mount park command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { mountPark { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, "mountPark").exists(_ === OperationOutcome.success)
    )

  }

  test("Process SCS follow command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { scsFollow(enable: true) { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, "scsFollow").exists(_ === OperationOutcome.success)
    )

  }

  test("Process command without obs id") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
                |mutation { slew (
                |  slewOptions: {
                |    zeroChopThrow: true
                |    zeroSourceOffset: true
                |    zeroSourceDiffTrack: true
                |    zeroMountOffset: true
                |    zeroMountDiffTrack: true
                |    shortcircuitTargetFilter: true
                |    shortcircuitMountFilter: true
                |    resetPointing: true
                |    stopGuide: true
                |    zeroGuideOffset: true
                |    zeroInstrumentOffset: true
                |    autoparkPwfs1: true
                |    autoparkPwfs2: true
                |    autoparkOiwfs: true
                |    autoparkGems: true
                |    autoparkAowfs: true
                |  },
                |  config: {
                |    sourceATarget: {
                |      id: "T0001"
                |      name: "Dummy"
                |      sidereal: {
                |        ra: {
                |          hms: "21:15:33"
                |        }
                |        dec: {
                |          dms: "-30:26:38"
                |        }
                |        epoch:"J2000.000"
                |     }
                |      wavelength: {
                |        nanometers: "400"
                |      }
                |    }
                |    instParams: {
                |      iaa: {
                |        degrees: 178.38
                |      }
                |      focusOffset: {
                |         micrometers: 1234
                |      }
                |      agName: "gmos"
                |      origin: {
                |        x: {
                |          milliarcseconds: 3012
                |        }
                |        y: {
                |          milliarcseconds: -1234
                |        }
                |      }
                |    }
                |    oiwfs: {
                |      target: {
                |        name: "OiwfsDummy"
                |        sidereal: {
                |          ra: {
                |            hms: "10:11:12"
                |          }
                |          dec: {
                |            dms: "-30:31:32"
                |          }
                |          epoch:"J2000.000"
                |        }
                |      }
                |      tracking: {
                |        nodAchopA: true
                |        nodAchopB: false
                |        nodBchopA: false
                |        nodBchopB: true
                |      }
                |    }
                |    rotator: {
                |      ipa: {
                |        degrees: 89.76
                |      }
                |      mode: TRACKING
                |    }
                |    instrument: GMOS_NORTH
                |    baffles: {
                |      manualConfig: {
                |        centralBaffle: OPEN
                |        deployableBaffle: VISIBLE
                |      }
                |    }
                |  },
                |  obsId: null
                |) {
                |  result
                |} }
                |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "slew").exists(_ === OperationOutcome.success)
    )
  }

  test("Process slew command with obs id") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
                |mutation { slew (
                |  slewOptions: {
                |    zeroChopThrow: true
                |    zeroSourceOffset: true
                |    zeroSourceDiffTrack: true
                |    zeroMountOffset: true
                |    zeroMountDiffTrack: true
                |    shortcircuitTargetFilter: true
                |    shortcircuitMountFilter: true
                |    resetPointing: true
                |    stopGuide: true
                |    zeroGuideOffset: true
                |    zeroInstrumentOffset: true
                |    autoparkPwfs1: true
                |    autoparkPwfs2: true
                |    autoparkOiwfs: true
                |    autoparkGems: true
                |    autoparkAowfs: true
                |  },
                |  config: {
                |    sourceATarget: {
                |      id: "T0001"
                |      name: "Dummy"
                |      sidereal: {
                |        ra: {
                |          hms: "21:15:33"
                |        }
                |        dec: {
                |          dms: "-30:26:38"
                |        }
                |        epoch:"J2000.000"
                |     }
                |      wavelength: {
                |        nanometers: "400"
                |      }
                |    }
                |    instParams: {
                |      iaa: {
                |        degrees: 178.38
                |      }
                |      focusOffset: {
                |         micrometers: 1234
                |      }
                |      agName: "gmos"
                |      origin: {
                |        x: {
                |          milliarcseconds: 3012
                |        }
                |        y: {
                |          milliarcseconds: -1234
                |        }
                |      }
                |    }
                |    oiwfs: {
                |      target: {
                |        name: "OiwfsDummy"
                |        sidereal: {
                |          ra: {
                |            hms: "10:11:12"
                |          }
                |          dec: {
                |            dms: "-30:31:32"
                |          }
                |          epoch:"J2000.000"
                |        }
                |      }
                |      tracking: {
                |        nodAchopA: true
                |        nodAchopB: false
                |        nodBchopA: false
                |        nodBchopB: true
                |      }
                |    }
                |    rotator: {
                |      ipa: {
                |        degrees: 89.76
                |      }
                |      mode: TRACKING
                |    }
                |    instrument: GMOS_NORTH
                |    baffles: {
                |      manualConfig: {
                |        centralBaffle: OPEN
                |        deployableBaffle: VISIBLE
                |      }
                |    }
                |  },
                |  obsId: "o-2446"
                |) {
                |  result
                |} }
                |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "slew").exists(_ === OperationOutcome.success)
    )
  }

  test("Process slew command with azimuth/elevation target") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { slew (
          |  slewOptions: {
          |    zeroChopThrow: true
          |    zeroSourceOffset: true
          |    zeroSourceDiffTrack: true
          |    zeroMountOffset: true
          |    zeroMountDiffTrack: true
          |    shortcircuitTargetFilter: true
          |    shortcircuitMountFilter: true
          |    resetPointing: true
          |    stopGuide: true
          |    zeroGuideOffset: true
          |    zeroInstrumentOffset: true
          |    autoparkPwfs1: true
          |    autoparkPwfs2: true
          |    autoparkOiwfs: true
          |    autoparkGems: true
          |    autoparkAowfs: true
          |  },
          |  config: {
          |    sourceATarget: {
          |      id: "T0001"
          |      name: "Dummy"
          |      azel: {
          |        azimuth: {
          |          dms: "01:15:33"
          |        }
          |        elevation: {
          |          dms: "-30:26:38"
          |        }
          |     }
          |      wavelength: {
          |        nanometers: "400"
          |      }
          |    }
          |    instParams: {
          |      iaa: {
          |        degrees: 178.38
          |      }
          |      focusOffset: {
          |         micrometers: 1234
          |      }
          |      agName: "gmos"
          |      origin: {
          |        x: {
          |          milliarcseconds: 3012
          |        }
          |        y: {
          |          milliarcseconds: -1234
          |        }
          |      }
          |    }
          |    rotator: {
          |      ipa: {
          |        degrees: 89.76
          |      }
          |      mode: TRACKING
          |    }
          |    instrument: GMOS_NORTH
          |    baffles: {
          |      manualConfig: {
          |        centralBaffle: OPEN
          |        deployableBaffle: VISIBLE
          |      }
          |    }
          |  },
          |  obsId: null
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "slew").exists(_ === OperationOutcome.success)
    )
  }

  test("Process slew command with ephemeris target") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { slew (
          |  slewOptions: {
          |    zeroChopThrow: true
          |    zeroSourceOffset: true
          |    zeroSourceDiffTrack: true
          |    zeroMountOffset: true
          |    zeroMountDiffTrack: true
          |    shortcircuitTargetFilter: true
          |    shortcircuitMountFilter: true
          |    resetPointing: true
          |    stopGuide: true
          |    zeroGuideOffset: true
          |    zeroInstrumentOffset: true
          |    autoparkPwfs1: true
          |    autoparkPwfs2: true
          |    autoparkOiwfs: true
          |    autoparkGems: true
          |    autoparkAowfs: true
          |  },
          |  config: {
          |    sourceATarget: {
          |      id: "T0001"
          |      name: "Dummy"
          |      nonsidereal: {
          |        keyType: ASTEROID_NEW
          |        des: "Dummy"
          |      }
          |    }
          |    instParams: {
          |      iaa: {
          |        degrees: 178.38
          |      }
          |      focusOffset: {
          |         micrometers: 1234
          |      }
          |      agName: "gmos"
          |      origin: {
          |        x: {
          |          milliarcseconds: 3012
          |        }
          |        y: {
          |          milliarcseconds: -1234
          |        }
          |      }
          |    }
          |    rotator: {
          |      ipa: {
          |        degrees: 89.76
          |      }
          |      mode: TRACKING
          |    }
          |    instrument: GMOS_NORTH
          |    baffles: {
          |      manualConfig: {
          |        centralBaffle: OPEN
          |        deployableBaffle: VISIBLE
          |      }
          |    }
          |  },
          |  obsId: null
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "slew").exists(_ === OperationOutcome.success)
    )
  }

  test("Process TCS configure command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { tcsConfig ( config: {
          |  sourceATarget: {
          |    id: "T0001"
          |    name: "Dummy"
          |    sidereal: {
          |      ra: {
          |        hms: "21:15:33"
          |      }
          |      dec: {
          |        dms: "-30:26:38"
          |      }
          |      epoch:"J2000.000"
          |   }
          |    wavelength: {
          |      nanometers: "400"
          |    }
          |  }
          |  instParams: {
          |    iaa: {
          |      degrees: 178.38
          |    }
          |    focusOffset: {
          |       micrometers: 1234
          |    }
          |    agName: "gmos"
          |    origin: {
          |      x: {
          |        milliarcseconds: 3012
          |      }
          |      y: {
          |        milliarcseconds: -1234
          |      }
          |    }
          |  }
          |  oiwfs: {
          |    target: {
          |      name: "OiwfsDummy"
          |      sidereal: {
          |        ra: {
          |          hms: "10:11:12"
          |        }
          |        dec: {
          |          dms: "-30:31:32"
          |        }
          |        epoch:"J2000.000"
          |      }
          |    }
          |    tracking: {
          |      nodAchopA: true
          |      nodAchopB: false
          |      nodBchopA: false
          |      nodBchopB: true
          |    }
          |  }
          |  rotator: {
          |    ipa: {
          |      microarcseconds: 89.76
          |    }
          |    mode: TRACKING
          |  }
          |  instrument: GMOS_NORTH
          |  baffles: {
          |    manualConfig: {
          |      centralBaffle: OPEN
          |      deployableBaffle: VISIBLE
          |    }
          |  }
          |} ) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "tcsConfig").exists(_ === OperationOutcome.success)
    )
  }

  test("Process swap target command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { swapTarget ( swapConfig: {
          |  guideTarget: {
          |    id: "T0001"
          |    name: "Dummy"
          |    sidereal: {
          |      ra: {
          |        hms: "21:15:33"
          |      }
          |      dec: {
          |        dms: "-30:26:38"
          |      }
          |      epoch:"J2000.000"
          |   }
          |    wavelength: {
          |      nanometers: "400"
          |    }
          |  }
          |  acParams: {
          |    iaa: {
          |      degrees: 178.38
          |    }
          |    focusOffset: {
          |       micrometers: 1234
          |    }
          |    agName: "ac"
          |    origin: {
          |      x: {
          |        milliarcseconds: 3012
          |      }
          |      y: {
          |        milliarcseconds: -1234
          |      }
          |    }
          |  }
          |  rotator: {
          |    ipa: {
          |      microarcseconds: 89.76
          |    }
          |    mode: TRACKING
          |  }
          |} ) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "swapTarget").exists(_ === OperationOutcome.success)
    )
  }

  test("Process restore target  command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { restoreTarget ( config: {
          |  sourceATarget: {
          |    id: "T0001"
          |    name: "Dummy"
          |    sidereal: {
          |      ra: {
          |        hms: "21:15:33"
          |      }
          |      dec: {
          |        dms: "-30:26:38"
          |      }
          |      epoch:"J2000.000"
          |   }
          |    wavelength: {
          |      nanometers: "400"
          |    }
          |  }
          |  instParams: {
          |    iaa: {
          |      degrees: 178.38
          |    }
          |    focusOffset: {
          |       micrometers: 1234
          |    }
          |    agName: "gmos"
          |    origin: {
          |      x: {
          |        milliarcseconds: 3012
          |      }
          |      y: {
          |        milliarcseconds: -1234
          |      }
          |    }
          |  }
          |  oiwfs: {
          |    target: {
          |      name: "OiwfsDummy"
          |      sidereal: {
          |        ra: {
          |          hms: "10:11:12"
          |        }
          |        dec: {
          |          dms: "-30:31:32"
          |        }
          |        epoch:"J2000.000"
          |      }
          |    }
          |    tracking: {
          |      nodAchopA: true
          |      nodAchopB: false
          |      nodBchopA: false
          |      nodBchopB: true
          |    }
          |  }
          |  rotator: {
          |    ipa: {
          |      microarcseconds: 89.76
          |    }
          |    mode: TRACKING
          |  }
          |  instrument: GMOS_NORTH
          |  baffles: {
          |    manualConfig: {
          |      centralBaffle: OPEN
          |      deployableBaffle: VISIBLE
          |    }
          |  }
          |} ) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "restoreTarget").exists(_ === OperationOutcome.success)
    )
  }

  test("Process instrumentSpecifics command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
                |mutation { instrumentSpecifics (instrumentSpecificsParams: {
                |  iaa: {
                |      microarcseconds: 123.432
                |    }
                |    focusOffset: {
                |      millimeters: 54.5432
                |    }
                |    agName: "Test"
                |    origin: {
                |      x: {
                |        arcseconds: 12.43
                |      }
                |      y: {
                |        arcseconds: 54.54
                |      }
                |    }
                |}) {
                |  result
                |} }
                |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "instrumentSpecifics").exists(
        _ === OperationOutcome.success
      )
    )
  }

  private def testWfsTarget(name: String): IO[Unit] =
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              s"""
                |mutation { ${name}Target (target: {
                |  id: "T0001"
                |  name: "Dummy"
                |  sidereal: {
                |    ra: {
                |      hms: "21:15:33"
                |    }
                |    dec: {
                |      dms: "-30:26:38"
                |    }
                |    epoch:"J2000.000"
                |  }
                |  wavelength: {
                |    nanometers: "400"
                |  }
                |}) {
                |  result
                |} }
                |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, s"${name}Target").exists(_ === OperationOutcome.success)
    )

  test("Process pwfs1Target command")(testWfsTarget("pwfs1"))

  test("Process pwfs2Target command")(testWfsTarget("pwfs2"))

  test("Process oiwfsTarget command")(testWfsTarget("oiwfs"))

  private def testWfsProbeTracking(name: String): IO[Unit] =
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              s"""
          |mutation { ${name}ProbeTracking (config: {
          |  nodAchopA: true
          |  nodAchopB: false
          |  nodBchopA: false
          |  nodBchopB: true
          |}) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, s"${name}ProbeTracking").exists(
        _ === OperationOutcome.success
      )
    )

  test("Process pwfs1ProbeTracking command")(testWfsProbeTracking("pwfs1"))

  test("Process pwfs2ProbeTracking command")(testWfsProbeTracking("pwfs2"))

  test("Process oiwfsProbeTracking command")(testWfsProbeTracking("oiwfs"))

  private def testWfsFollow(name: String): IO[Unit] =
    for {
      mp <- buildMapping()

      r <- mp.compileAndRun(s"mutation { ${name}Follow(enable: true) { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, s"${name}Follow").exists(_ === OperationOutcome.success)
    )

  test("Process pwfs1 follow command")(testWfsFollow("pwfs1"))

  test("Process pwfs2 follow command")(testWfsFollow("pwfs2"))

  test("Process oiwfs follow command")(testWfsFollow("oiwfs"))

  private def testWfsPark(name: String): IO[Unit] =
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(s"mutation { ${name}Park { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, s"${name}Park").exists(_ === OperationOutcome.success)
    )

  test("Process pwfs1 park command")(testWfsPark("pwfs1"))

  test("Process pwfs2 park command")(testWfsPark("pwfs2"))

  test("Process oiwfs park command")(testWfsPark("oiwfs"))

  test("Process rotator follow command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { rotatorFollow(enable: true) { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, "rotatorFollow").exists(_ === OperationOutcome.success)
    )
  }

  test("Process rotator park command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { rotatorPark { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, "rotatorPark").exists(_ === OperationOutcome.success)
    )
  }

  test("Process rotator tracking configuration command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { rotatorConfig( config: {
          |    ipa: {
          |      microarcseconds: 89.76
          |    }
          |    mode: TRACKING
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "rotatorConfig").exists(_ === OperationOutcome.success)
    )
  }

  test("Provide logs subscription") {
    val infoMsg: String    = "info message"
    val warningMsg: String = "warning message"
    val errorMsg: String   = "error message"

    val logEvents = List(
      SimpleLoggingEvent(Timestamp.Min, Level.INFO, "", infoMsg),
      SimpleLoggingEvent(Timestamp.Min, Level.WARN, "", warningMsg),
      SimpleLoggingEvent(Timestamp.Min, Level.ERROR, "", errorMsg)
    )

    def putLogs(topic: Topic[IO, ILoggingEvent]): IO[Unit] =
      logEvents.map(topic.publish1).sequence.void

    for {
      mp   <- buildMapping()
      logs <- mp.compileAndRunSubscription(
                """
          | subscription {
          |   logMessage {
          |     timestamp
          |     level
          |     thread
          |     message
          |   }
          | }
          |""".stripMargin
              ).map(_.hcursor.downField("data").downField("logMessage").as[SimpleLoggingEvent])
                .take(logEvents.length)
                .compile
                .toList
                .timeout(Duration.fromNanos(10e9))
                .both(putLogs(mp.logTopic).delayBy(Duration.fromNanos(1e9)))
                .map(_._1.collect { case Right(a) => a })
    } yield {
      assert(logs.exists(_.message === infoMsg))
      assert(logs.exists(_.message === warningMsg))
      assert(logs.exists(_.message === errorMsg))
      assertEquals(logEvents, logEvents)
    }
  }

  test("Persist log messages subscription") {
    val debugMsg: String   = "debug message"
    val infoMsg: String    = "info message"
    val warningMsg: String = "warning message"

    val logEvents       = List(
      SimpleLoggingEvent(Timestamp.Min, Level.INFO, "", infoMsg),
      SimpleLoggingEvent(Timestamp.Min, Level.WARN, "", warningMsg)
    )
    val bufferedMessage = SimpleLoggingEvent(Timestamp.Min, Level.DEBUG, "", debugMsg)

    def putLogs(topic: Topic[IO, ILoggingEvent]): IO[Unit] =
      logEvents.map(topic.publish1).sequence.void

    for {
      mp   <- buildMapping()
      _    <- mp.logBuffer.set(Seq(bufferedMessage))
      logs <- mp.compileAndRunSubscription(
                """
          | subscription {
          |   logMessage {
          |     timestamp
          |     level
          |     thread
          |     message
          |   }
          | }
          |""".stripMargin
              ).map(_.hcursor.downField("data").downField("logMessage").as[SimpleLoggingEvent])
                .take(logEvents.length + 1)
                .compile
                .toList
                .timeout(Duration.fromNanos(10e9))
                .both(putLogs(mp.logTopic).delayBy(Duration.fromNanos(1e9)))
                .map(_._1.collect { case Right(a) => a })
    } yield assertEquals(logs, bufferedMessage +: logEvents)
  }

  test("Provide guide state subscription") {

    val changes: List[GuideState] = List(
      GuideState(
        MountGuideOption.MountGuideOn,
        M1GuideConfig.M1GuideOn(M1Source.OIWFS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS)),
        none,
        false,
        false,
        true,
        false
      ),
      GuideState(MountGuideOption.MountGuideOff,
                 M1GuideConfig.M1GuideOff,
                 M2GuideConfig.M2GuideOff,
                 none,
                 false,
                 false,
                 false,
                 false
      ),
      GuideState(
        MountGuideOption.MountGuideOff,
        M1GuideConfig.M1GuideOn(M1Source.OIWFS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS)),
        ProbeGuide(GuideProbe.GmosOIWFS, GuideProbe.GmosOIWFS).some,
        false,
        false,
        true,
        false
      ),
      GuideState(
        MountGuideOption.MountGuideOff,
        M1GuideConfig.M1GuideOn(M1Source.OIWFS),
        M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS)),
        none,
        false,
        false,
        true,
        false
      )
    )

    def putGuideUpdates(topic: Topic[IO, GuideState]): IO[Unit] =
      changes.map(topic.publish1).sequence.void

    val s: IO[List[Result[GuideState]]] = for {
      mp <- buildMapping()
      up <- mp.compileAndRunSubscription(
              """
            | subscription {
            |   guideState {
            |     m2Inputs
            |     m2Coma
            |     m1Input
            |     mountOffload
            |     probeGuide {
            |       from
            |       to
            |     }
            |     p1Integrating
            |     p2Integrating
            |     oiIntegrating
            |     acIntegrating
            |   }
            | }
            |""".stripMargin
            ).map(_.hcursor.downField("data").downField("guideState").as[GuideState])
              .take(changes.length)
              .compile
              .toList
              .timeout(Duration.fromNanos(10e9))
              .both(putGuideUpdates(mp.guideStateTopic).delayBy(Duration.fromNanos(1e9)))
              .map(_._1)
    } yield up

    s.map { l =>
      val g: List[GuideState] = l.collect { case Right(a) => a }
      assertEquals(g.length, changes.length)
      assertEquals(g, changes)
    }

  }

  test("Query telescope state") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   telescopeState {
          |     mount {
          |       parked
          |       follow
          |     }
          |     scs {
          |       parked
          |       follow
          |     }
          |     crcs {
          |       parked
          |       follow
          |     }
          |     pwfs1 {
          |       parked
          |       follow
          |     }
          |     pwfs2 {
          |       parked
          |       follow
          |     }
          |     oiwfs {
          |       parked
          |       follow
          |     }
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(r.hcursor.downField("data").downField("telescopeState").as[TelescopeState],
                         TelescopeState.default.asRight[DecodingFailure]
    )
  }

  test("Query guide state") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   guideState {
          |     m2Inputs
          |     m2Coma
          |     m1Input
          |     mountOffload
          |     p1Integrating
          |     p2Integrating
          |     oiIntegrating
          |     acIntegrating
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(r.hcursor.downField("data").downField("guideState").as[GuideState],
                         GuideState.default.asRight[DecodingFailure]
    )
  }

  test("Query WFS guide quality") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   guidersQualityValues {
          |     pwfs1 {
          |       flux
          |       centroidDetected
          |     }
          |     pwfs2 {
          |       flux
          |       centroidDetected
          |     }
          |     oiwfs {
          |       flux
          |       centroidDetected
          |     }
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("guidersQualityValues").as[GuidersQualityValues],
      GuidersQualityValues.default.asRight[DecodingFailure]
    )
  }

  test("Query Navigate server state") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   navigateState {
          |     onSwappedTarget
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(r.hcursor.downField("data").downField("navigateState").as[NavigateState],
                         NavigateState.default.asRight[DecodingFailure]
    )
  }

  test("Provide telescope state subscription") {

    val changes: List[TelescopeState] = List(
      TelescopeState(
        MechSystemState(NotParked, Following),
        MechSystemState(NotParked, Following),
        MechSystemState(NotParked, Following),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(NotParked, Following)
      ),
      TelescopeState(
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing)
      ),
      TelescopeState(
        MechSystemState(NotParked, Following),
        MechSystemState(NotParked, Following),
        MechSystemState(NotParked, Following),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(Parked, NotFollowing),
        MechSystemState(NotParked, Following)
      )
    )

    def putTelescopeUpdates(topic: Topic[IO, TelescopeState]): IO[Unit] =
      changes.map(topic.publish1).sequence.void

    val s: IO[List[Result[TelescopeState]]] = for {
      mp <- buildMapping()
      up <- mp.compileAndRunSubscription(
              """
            | subscription {
            |   telescopeState {
            |     mount {
            |       parked
            |       follow
            |     }
            |     scs {
            |       parked
            |       follow
            |     }
            |     crcs {
            |       parked
            |       follow
            |     }
            |     pwfs1 {
            |       parked
            |       follow
            |     }
            |     pwfs2 {
            |       parked
            |       follow
            |     }
            |     oiwfs {
            |       parked
            |       follow
            |     }
            |   }
            | }
            |""".stripMargin
            ).map(_.hcursor.downField("data").downField("telescopeState").as[TelescopeState])
              .take(changes.length)
              .compile
              .toList
              .timeout(Duration.fromNanos(10e9))
              .both(putTelescopeUpdates(mp.telescopeStateTopic).delayBy(Duration.fromNanos(1e9)))
              .map(_._1)
    } yield up

    s.map { l =>
      val g: List[TelescopeState] = l.collect { case Right(a) => a }
      assertEquals(g.length, changes.length)
      assertEquals(g, changes)
    }

  }

  test("Provide acquisition adjustment state subscription") {
    import lucuma.core.math.Angle
    import lucuma.core.math.Offset
    import navigate.model.enums.AcquisitionAdjustmentCommand

    val changes: List[AcquisitionAdjustment] = List(
      AcquisitionAdjustment(
        offset = Offset.signedDecimalArcseconds.reverseGet(2, 3),
        ipa = Angle.fromDoubleArcseconds(0.1).some,
        iaa = Angle.fromDoubleArcseconds(0.2).some,
        command = AcquisitionAdjustmentCommand.AskUser
      ),
      AcquisitionAdjustment(
        offset = Offset.signedDecimalArcseconds.reverseGet(4, 5),
        ipa = Angle.fromDoubleArcseconds(0.2).some,
        iaa = Angle.fromDoubleArcseconds(0.3).some,
        command = AcquisitionAdjustmentCommand.UserConfirms
      ),
      AcquisitionAdjustment(
        offset = Offset.signedDecimalArcseconds.reverseGet(5, 6),
        ipa = None,
        iaa = None,
        command = AcquisitionAdjustmentCommand.AskUser
      )
    )

    def putAcquisitionAdjustmentUpdates(topic: Topic[IO, AcquisitionAdjustment]): IO[Unit] =
      changes.map(topic.publish1).sequence.void

    val s: IO[List[Result[AcquisitionAdjustment]]] = for {
      mp <- buildMapping()
      up <- mp.compileAndRunSubscription(
              """
             | subscription {
             |   acquisitionAdjustmentState {
             |     offset {
             |       p {
             |         milliarcseconds
             |       }
             |       q {
             |         milliarcseconds
             |       }
             |     }
             |     ipa {
             |       microarcseconds
             |     }
             |     iaa {
             |       milliarcseconds
             |     }
             |     command
             |   }
             | }
             |""".stripMargin
            ).map { a =>
              a.hcursor
                .downField("data")
                .downField("acquisitionAdjustmentState")
                .as[AcquisitionAdjustment]
            }.take(changes.length)
              .compile
              .toList
              .timeout(Duration.fromNanos(10e9))
              .both(
                putAcquisitionAdjustmentUpdates(mp.acquisitionAdjustmentTopic)
                  .delayBy(Duration.fromNanos(1e9))
              )
              .map(_._1)
    } yield up

    s.map { l =>
      val g: List[AcquisitionAdjustment] = l.collect { case Right(a) => a }
      assertEquals(g.length, changes.length)
      assertEquals(g, changes)
    }
  }

  test("Process guide disable command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { guideDisable { result } }")
    } yield assert(
      extractResult[OperationOutcome](r, "guideDisable").exists(_ === OperationOutcome.success)
    )
  }

  test("Process guide enable command") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { guideEnable( config: {
          |    m2Inputs: [ OIWFS ]
          |    m2Coma: true
          |    m1Input: OIWFS
          |    mountOffload: true
          |    daytimeMode: false
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "guideEnable").exists(_ === OperationOutcome.success)
    )
  }

  private def testWfsObserve(name: String): IO[Unit] =
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              s"""
          |mutation { ${name}Observe( period: {
          |    milliseconds: 20
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, s"${name}Observe").exists(_ === OperationOutcome.success)
    )

  test("Process pwfs1 observe command")(testWfsObserve("pwfs1"))

  test("Process pwfs2 observe command")(testWfsObserve("pwfs2"))

  test("Process oiwfs observe command")(testWfsObserve("oiwfs"))

  test("Process ac observe command")(testWfsObserve("ac"))

  private def testWfsStopObserve(name: String): IO[Unit] =
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              s"""
          |mutation { ${name}StopObserve {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, s"${name}StopObserve")
        .exists(_ === OperationOutcome.success)
    )

  test("Process pwfs1 stop observe command")(testWfsStopObserve("pwfs1"))

  test("Process pwfs2 stop observe command")(testWfsStopObserve("pwfs2"))

  test("Process oiwfs stop observe command")(testWfsStopObserve("oiwfs"))

  test("Process ac stop observe command")(testWfsStopObserve("ac"))

  def m1Test(name: String, mutation: String) =
    test(s"Process M1 $name command") {
      for {
        mp <- buildMapping()
        r  <- mp.compileAndRun(
                s"""
            |mutation { $mutation {
            |  result
            |} }
            |""".stripMargin
              )
      } yield assert(
        extractResult[OperationOutcome](r, mutation).exists(_ === OperationOutcome.success)
      )
    }

  val m1ParkTest: Unit = m1Test("park", "m1Park")

  val m2UnparkTest: Unit = m1Test("unpark", "m1Unpark")

  val m1OpenLoopOffTest: Unit = m1Test("open loop off", "m1OpenLoopOff")

  val m1OpenLoopOnTest: Unit = m1Test("open loop on", "m1OpenLoopOn")

  val m1ZeroFigureTest: Unit = m1Test("zero figure", "m1ZeroFigure")

  val m1LoadAoFigureTest: Unit = m1Test("load AO figure", "m1LoadAoFigure")

  val m1LoanNonAoFigureTest: Unit = m1Test("load non AO figure", "m1LoadNonAoFigure")

  test("Set probeGuide OIWFS to OIWFS") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { guideEnable( config: {
          |    m2Inputs: [ OIWFS ]
          |    m2Coma: true
          |    m1Input: OIWFS
          |    mountOffload: true
          |    daytimeMode: false
          |    probeGuide: {
          |      from: GMOS_OIWFS
          |      to: GMOS_OIWFS
          |    }
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "guideEnable").exists(_ === OperationOutcome.success)
    )
  }

  test("Set probeGuide PWFS1 to PWFS2") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          |mutation { guideEnable( config: {
          |    m2Inputs: [ OIWFS ]
          |    m2Coma: true
          |    m1Input: OIWFS
          |    mountOffload: true
          |    daytimeMode: false
          |    probeGuide: {
          |      from: PWFS1
          |      to: PWFS2
          |    }
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assert(
      extractResult[OperationOutcome](r, "guideEnable").exists(_ === OperationOutcome.success)
    )
  }

  test("Configure light path") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation { lightpathConfig (
          |  from: SKY,
          |  to: GMOS
          |) {
          |  result
          |} }
          |""".stripMargin
            )
      q  <- mp.compileAndRun(
              """
          |mutation { lightpathConfig (
          |  from: AO,
          |  to: GMOS
          |) {
          |  result
          |} }
          |""".stripMargin
            )
      r  <- mp.compileAndRun(
              """
          |mutation { lightpathConfig (
          |  from: GCAL,
          |  to: GMOS
          |) {
          |  result
          |} }
          |""".stripMargin
            )
      s  <- mp.compileAndRun(
              """
          |mutation { lightpathConfig (
          |  from: SKY,
          |  to: AC
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield {
      assert(
        extractResult[OperationOutcome](p, "lightpathConfig").exists(_ === OperationOutcome.success)
      )
      assert(
        extractResult[OperationOutcome](q, "lightpathConfig").exists(_ === OperationOutcome.success)
      )
      assert(
        extractResult[OperationOutcome](r, "lightpathConfig").exists(_ === OperationOutcome.success)
      )
      assert(
        extractResult[OperationOutcome](s, "lightpathConfig").exists(_ === OperationOutcome.success)
      )
    }
  }

  test("Get instrument port") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |query {
          |  instrumentPort( instrument: GMOS_NORTH )
          |}
          |""".stripMargin
            )
      q  <- mp.compileAndRun(
              """
          |query {
          |  instrumentPort( instrument: NICI )
          |}
          |""".stripMargin
            )
    } yield {
      assertEquals(p.hcursor.downField("data").downField("instrumentPort").as[Int].toOption, 5.some)
      assertEquals(q.hcursor.downField("data").downField("instrumentPort").as[Int].toOption, none)
    }
  }

  test("request acquisition adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation { acquisitionAdjustment (
          |  adjustment: {
          |    offset: {
          |      p: {
          |        arcseconds: 0.1
          |      }
          |      q: {
          |        arcseconds: 0.1
          |      }
          |    },
          |    ipa: {
          |       milliseconds: 10
          |    },
          |    iaa: {
          |       milliseconds: 10
          |    }
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("acquisitionAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("confirm request acquisition adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation { acquisitionAdjustment (
          |  adjustment: {
          |    offset: {
          |      p: {
          |        arcseconds: 0.1
          |      }
          |      q: {
          |        arcseconds: 0.1
          |      }
          |    },
          |    ipa: {
          |       milliseconds: 10
          |    },
          |    iaa: {
          |       milliseconds: 10
          |    },
          |    command: USER_CONFIRMS
          |  }
          |) {
          |  result
          |} }
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("acquisitionAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Get server version") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |query {
          |  serverVersion
          |}
          |""".stripMargin
            )
    } yield assertEquals(p.hcursor.downField("data").downField("serverVersion").as[String].toOption,
                         OcsBuildInfo.version.some
    )
  }

  test("Take WFS sky") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  wfsSky(
          |    wfs: GMOS_OIWFS,
          |    period: {
          |      milliseconds: 20
          |    }
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor.downField("data").downField("wfsSky").downField("result").as[String].toOption,
      "SUCCESS".some
    )
  }

  test("Adjust target position") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustTarget(
          |    target: SOURCE_A,
          |    offset: {
          |      focalPlaneAdjustment: {
          |        deltaX: {
          |          arcseconds: 0.1
          |        }
          |        deltaY: {
          |          arcseconds: 0.0
          |        }
          |      }
          |    }
          |    openLoops: true
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor.downField("data").downField("adjustTarget").downField("result").as[String].toOption,
      "SUCCESS".some
    )
  }

  test("Adjust pointing") {
    def checkResult(j: Json): Option[String] =
      j.hcursor
        .downField("data")
        .downField("adjustPointing")
        .downField("result")
        .as[String]
        .toOption

    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustPointing(
          |    offset: {
          |      focalPlaneAdjustment: {
          |        deltaX: {
          |          arcseconds: 0.1
          |        }
          |        deltaY: {
          |          arcseconds: 0.0
          |        }
          |      }
          |    }
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
      q  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustPointing(
          |    offset: {
          |      equatorialAdjustment: {
          |        deltaRA: {
          |          arcseconds: 0.1
          |        }
          |        deltaDec: {
          |          arcseconds: 0.0
          |        }
          |      }
          |    }
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
      r  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustPointing(
          |    offset: {
          |      instrumentAdjustment: {
          |        p: {
          |          arcseconds: 0.1
          |        }
          |        q: {
          |          arcseconds: 0.1
          |        },
          |      }
          |    }
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
      s  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustPointing(
          |    offset: {
          |      horizontalAdjustment: {
          |        azimuth: {
          |          arcseconds: 0.1
          |        }
          |        elevation: {
          |          arcseconds: 0.0
          |        }
          |      }
          |    }
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
      t  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustPointing(
          |    offset: {
          |      probeFrameAdjustment: {
          |        probeFrame: GMOS_OIWFS
          |        deltaU: {
          |          arcseconds: 0.1
          |        }
          |        deltaV: {
          |          arcseconds: 0.0
          |        }
          |        alignAngle: {
          |          arcseconds: 357.2
          |        }
          |      }
          |    }
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield {
      assertEquals(checkResult(p), "SUCCESS".some)
      assertEquals(checkResult(q), "SUCCESS".some)
      assertEquals(checkResult(r), "SUCCESS".some)
      assertEquals(checkResult(s), "SUCCESS".some)
      assertEquals(checkResult(t), "SUCCESS".some)
    }
  }

  test("Adjust instrument origin") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  adjustOrigin(
          |    offset: {
          |      focalPlaneAdjustment: {
          |        deltaX: {
          |          arcseconds: 0.1
          |        }
          |        deltaY: {
          |          arcseconds: 0.0
          |        }
          |      }
          |    }
          |    openLoops: true
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor.downField("data").downField("adjustOrigin").downField("result").as[String].toOption,
      "SUCCESS".some
    )
  }

  test("Clear target adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  resetTargetAdjustment(
          |    target: SOURCE_A
          |    openLoops: true
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("resetTargetAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Absorb target adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  absorbTargetAdjustment(
          |    target: SOURCE_A
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("absorbTargetAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Clear local pointing adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  resetLocalPointingAdjustment {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("resetLocalPointingAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Clear guide pointing adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  resetGuidePointingAdjustment {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("resetGuidePointingAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Absorb guide pointing adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  absorbGuidePointingAdjustment {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("absorbGuidePointingAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Clear origin adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  resetOriginAdjustment (
          |    openLoops: true
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("resetOriginAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Absorb origin adjustment") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  absorbOriginAdjustment {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("absorbOriginAdjustment")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Query target offsets") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   targetAdjustmentOffsets {
          |     sourceA {
          |       deltaX {
          |         milliarcseconds
          |       }
          |       deltaY {
          |         milliarcseconds
          |       }
          |     }
          |     pwfs1 {
          |       deltaX {
          |         milliarcseconds
          |       }
          |       deltaY {
          |         milliarcseconds
          |       }
          |     }
          |     pwfs2 {
          |       deltaX {
          |         milliarcseconds
          |       }
          |       deltaY {
          |         milliarcseconds
          |       }
          |     }
          |     oiwfs {
          |       deltaX {
          |         milliarcseconds
          |       }
          |       deltaY {
          |         milliarcseconds
          |       }
          |     }
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("targetAdjustmentOffsets").as[TargetOffsets],
      TargetOffsets.default.asRight[DecodingFailure]
    )
  }

  test("Query pointing offset") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   pointingAdjustmentOffset {
          |     local {
          |       azimuth {
          |         milliarcseconds
          |       }
          |       elevation {
          |         milliarcseconds
          |       }
          |     }
          |     guide {
          |       azimuth {
          |         milliarcseconds
          |       }
          |       elevation {
          |         milliarcseconds
          |       }
          |     }
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("pointingAdjustmentOffset").as[PointingCorrections],
      PointingCorrections.default.asRight[DecodingFailure]
    )
  }

  test("Query instrument origin offset") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query {
          |   originAdjustmentOffset {
          |     deltaX {
          |       milliarcseconds
          |     }
          |     deltaY {
          |       milliarcseconds
          |     }
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("originAdjustmentOffset").as[FocalPlaneOffset],
      FocalPlaneOffset.Zero.asRight[DecodingFailure]
    )
  }

  test("Query server configuration") {
    val expected = ServerConfiguration(OcsBuildInfo.version, Site.GS, "ws://odb", "https://sso")
    val conf     = NavigateConfiguration.default
      .focus(_.site)
      .replace(expected.site)
      .focus(_.navigateEngine.odb)
      .replace(Uri.unsafeFromString(expected.odbUri))
      .focus(_.lucumaSSO.ssoUrl)
      .replace(Uri.unsafeFromString(expected.ssoUri))

    for {
      mp <- buildMapping(conf)
      r  <- mp.compileAndRun(
              """
          | query {
          |   serverConfiguration {
          |     version
          |     site
          |     odbUri
          |     ssoUri
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("serverConfiguration").as[ServerConfiguration],
      expected.asRight[DecodingFailure]
    )
  }

  test("Notify server errors") {
    for {
      mp <- buildMapping(NavigateConfiguration.default, buildBadServer)
      r  <- mp.compileAndRun("mutation { oiwfsPark { result } }")
    } yield assertEquals(
      r.hcursor.downField("errors").downArray.downField("message").as[String],
      "Error".asRight[DecodingFailure]
    )
  }

  test("Query AC mechanisms state") {
    val expected = AcMechsState(AcLens.Ac.some, AcNdFilter.Open.some, AcFilter.Neutral.some)
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
        | query { acMechsState {
        |     lens
        |     filter
        |     ndFilter
        |   }
        | }
        |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("acMechsState").as[AcMechsState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Query AC mechanisms state whit unknown positions") {
    val expected = AcMechsState(none, none, none)
    for {
      mp <- buildMapping(NavigateConfiguration.default, buildServerUndefinedPos)
      r  <- mp.compileAndRun(
              """
          | query { acMechsState {
          |     lens
          |     filter
          |     ndFilter
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("acMechsState").as[AcMechsState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Set AC lens") {
    Enumerated[AcLens].all.map { v =>
      for {
        mp <- buildMapping()
        p  <- mp.compileAndRun(
                s"""
             |mutation {
             |  acLens(lens: ${v.tag.toScreamingSnakeCase}) {
             |    result
             |  }
             |}
             |""".stripMargin
              )
      } yield assertEquals(
        p.hcursor
          .downField("data")
          .downField("acLens")
          .downField("result")
          .as[String]
          .toOption,
        "SUCCESS".some
      )
    }.sequence
  }

  test("Set AC filter") {
    Enumerated[AcFilter].all.map { v =>
      for {
        mp <- buildMapping()
        p  <- mp.compileAndRun(
                s"""
            |mutation {
            |  acFilter(filter: ${v.tag.toScreamingSnakeCase}) {
            |    result
            |  }
            |}
            |""".stripMargin
              )
      } yield assertEquals(
        p.hcursor
          .downField("data")
          .downField("acFilter")
          .downField("result")
          .as[String]
          .toOption,
        "SUCCESS".some
      )
    }.sequence
  }

  test("Set AC ND Filter") {
    Enumerated[AcNdFilter].all.map { v =>
      for {
        mp <- buildMapping()
        p  <- mp.compileAndRun(
                s"""
            |mutation {
            |  acNdFilter(ndFilter: ${v.tag.toScreamingSnakeCase}) {
            |    result
            |  }
            |}
            |""".stripMargin
              )
      } yield assertEquals(
        p.hcursor
          .downField("data")
          .downField("acNdFilter")
          .downField("result")
          .as[String]
          .toOption,
        "SUCCESS".some
      )
    }.sequence
  }

  test("Set AC window") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
            |mutation {
            |  acWindowSize(size: {
            |    type: WINDOW_200X200
            |    center: {
            |      x: 123
            |      y: 456
            |    }
            |  }) {
            |    result
            |  }
            |}
            |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("acWindowSize")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Query Pwfs1 mechanisms state") {
    val expected = PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some)
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query { pwfs1MechsState {
          |     filter
          |     fieldStop
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("pwfs1MechsState").as[PwfsMechsState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Query Pwfs1 mechanisms state with unknown positions") {
    val expected = PwfsMechsState(none, none)
    for {
      mp <- buildMapping(NavigateConfiguration.default, buildServerUndefinedPos)
      r  <- mp.compileAndRun(
              """
          | query { pwfs1MechsState {
          |     filter
          |     fieldStop
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("pwfs1MechsState").as[PwfsMechsState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Set Pwfs1 filter") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  pwfs1Filter(
          |    filter: GREEN
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("pwfs1Filter")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Set Pwfs1 field stop") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  pwfs1FieldStop(
          |    fieldStop: FS1_6
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("pwfs1FieldStop")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Query Pwfs2 mechanisms state") {
    val expected = PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some)
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
          | query { pwfs2MechsState {
          |     filter
          |     fieldStop
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("pwfs2MechsState").as[PwfsMechsState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Query Pwfs2 mechanisms state with unknown positions") {
    val expected = PwfsMechsState(none, none)
    for {
      mp <- buildMapping(NavigateConfiguration.default, buildServerUndefinedPos)
      r  <- mp.compileAndRun(
              """
          | query { pwfs2MechsState {
          |     filter
          |     fieldStop
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("pwfs2MechsState").as[PwfsMechsState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Set Pwfs2 filter") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  pwfs2Filter(
          |    filter: NEUTRAL
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("pwfs2Filter")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Set Pwfs2 field stop") {
    for {
      mp <- buildMapping()
      p  <- mp.compileAndRun(
              """
          |mutation {
          |  pwfs2FieldStop(
          |    fieldStop: FS3_2
          |  ) {
          |    result
          |  }
          |}
          |""".stripMargin
            )
    } yield assertEquals(
      p.hcursor
        .downField("data")
        .downField("pwfs2FieldStop")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Query M2 baffle positions") {
    val expected = BafflesState(CentralBafflePosition.Open, DeployableBafflePosition.Visible)
    for {
      mp <- buildMapping(NavigateConfiguration.default, buildServerUndefinedPos)
      r  <- mp.compileAndRun(
              """
          | query { bafflesState {
          |     centralBaffle
          |     deployableBaffle
          |   }
          | }
          |""".stripMargin
            )
    } yield assertEquals(
      r.hcursor.downField("data").downField("bafflesState").as[BafflesState],
      expected.asRight[DecodingFailure]
    )
  }

  test("Set PWFS1 circular buffer") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { pwfs1CircularBuffer( enable: true ) { result } }")
    } yield assertEquals(
      r.hcursor
        .downField("data")
        .downField("pwfs1CircularBuffer")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Set PWFS2 circular buffer") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { pwfs2CircularBuffer( enable: true ) { result } }")
    } yield assertEquals(
      r.hcursor
        .downField("data")
        .downField("pwfs2CircularBuffer")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Set OIWFS circular buffer") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun("mutation { oiwfsCircularBuffer( enable: true ) { result } }")
    } yield assertEquals(
      r.hcursor
        .downField("data")
        .downField("oiwfsCircularBuffer")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

  test("Refresh ephemeris files in TCS") {
    for {
      mp <- buildMapping()
      r  <- mp.compileAndRun(
              """
        | mutation {
        |   refreshEphemerisFiles(
        |     observingNight: "2026-01-01"
        |   ) { result } }
        """.stripMargin
            )
    } yield assertEquals(
      r.hcursor
        .downField("data")
        .downField("refreshEphemerisFiles")
        .downField("result")
        .as[String]
        .toOption,
      "SUCCESS".some
    )
  }

}

object NavigateMappingsTest {
  import lucuma.odb.json.offset.query.given
  import lucuma.odb.json.angle.query.given

  val dummyClient = Client.fromHttpApp(HttpApp.notFound[IO])

  private given Logger[IO] = NoOpLogger.impl[IO]

  class NavigateEngineTest(
    tcsSouth: TcsSouthController[IO],
    tcsNorth: TcsNorthController[IO],
    guideRef: Ref[IO, GuideConfig]
  ) extends NavigateEngine[IO] {

    override val systems: Systems[IO] = Systems(
      OdbProxy.dummy[IO],
      dummyClient,
      HorizonsClient(dummyClient),
      tcsSouth,
      tcsSouth,
      tcsNorth
    )

    override def eventStream: Stream[IO, NavigateEvent] = Stream.empty

    override def mcsPark: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def mcsFollow(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def rotStop(useBrakes: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def rotPark: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def rotFollow(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def rotMove(angle: RotatorAngle): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def ecsCarouselMode(
      domeMode:      DomeMode,
      shutterMode:   ShutterMode,
      slitHeight:    Double,
      domeEnable:    Boolean,
      shutterEnable: Boolean
    ): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def ecsVentGatesMove(gateEast: Double, westGate: Double): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def slew(
      slewOptions: SlewOptions,
      config:      TcsConfig,
      oid:         Option[Observation.Id]
    ): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def instrumentSpecifics(
      instrumentSpecificsParams: InstrumentSpecifics
    ): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs1Target(target: Target): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs1ProbeTracking(config: TrackingConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs1Park: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def pwfs1Follow(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2Target(target: Target): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2ProbeTracking(config: TrackingConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2Park: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def pwfs2Follow(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def oiwfsTarget(target: Target): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def oiwfsProbeTracking(config: TrackingConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def oiwfsPark: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def oiwfsFollow(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def rotTrackingConfig(cfg: RotatorTrackConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def enableGuide(config: TelescopeGuideConfig): IO[CommandResult] =
      tcsSouth.enableGuide(config).as(CommandResult.CommandSuccess)

    override def disableGuide: IO[CommandResult] =
      tcsSouth.disableGuide.as(CommandResult.CommandSuccess)

    override def tcsConfig(config: TcsConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs1Observe(period: TimeSpan): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs1StopObserve: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def pwfs2Observe(period: TimeSpan): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2StopObserve: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def oiwfsObserve(period: TimeSpan): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def oiwfsStopObserve: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def acObserve(period: TimeSpan): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def acStopObserve: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def getGuideState: IO[GuideState] = tcsSouth.getGuideState

    override def getGuidersQuality: IO[GuidersQualityValues] = GuidersQualityValues.default.pure[IO]

    override def getTelescopeState: IO[TelescopeState] = tcsSouth.getTelescopeState

    override def scsFollow(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def swapTarget(swapConfig: SwapConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def restoreTarget(config: TcsConfig): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def getNavigateState: IO[NavigateState] = NavigateState.default.pure[IO]

    override def getNavigateStateStream: Stream[IO, NavigateState] =
      Stream.eval(NavigateState.default.pure[IO])

    override def m1Park: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def m1Unpark: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def m1OpenLoopOff: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def m1OpenLoopOn: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def m1ZeroFigure: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def m1LoadAoFigure: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def m1LoadNonAoFigure: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def lightpathConfig(from: LightSource, to: LightSinkName): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def getInstrumentPort(instrument: Instrument): IO[Option[Int]] = (instrument match
      case Instrument.GmosNorth => 5.some
      case _                    => none
    ).pure[IO]

    override def acquisitionAdj(
      offset: Offset,
      iaa:    Option[Angle],
      ipa:    Option[Angle]
    ): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def getGuideDemand: IO[GuideConfig] = guideRef.get

    def getTargetAdjustments: IO[TargetOffsets] = TargetOffsets.default.pure[IO]

    override def wfsSky(wfs: GuideProbe, period: TimeSpan): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def getPointingOffset: IO[PointingCorrections] = PointingCorrections.default.pure[IO]

    override def getOriginOffset: IO[FocalPlaneOffset] = FocalPlaneOffset.Zero.pure[IO]

    override def targetAdjust(
      target:            VirtualTelescope,
      handsetAdjustment: HandsetAdjustment,
      openLoops:         Boolean
    ): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def originAdjust(
      handsetAdjustment: HandsetAdjustment,
      openLoops:         Boolean
    ): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def pointingAdjust(handsetAdjustment: HandsetAdjustment): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def targetOffsetAbsorb(target: VirtualTelescope): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def targetOffsetClear(
      target:    VirtualTelescope,
      openLoops: Boolean
    ): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def originOffsetAbsorb: IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def originOffsetClear(openLoops: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pointingOffsetClearLocal: IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pointingOffsetAbsorbGuide: IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pointingOffsetClearGuide: IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def acLens(l: AcLens): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def acNdFilter(nd: AcNdFilter): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def acFilter(flt: AcFilter): IO[CommandResult] = CommandResult.CommandSuccess.pure[IO]

    override def acWindowSize(wnd: AcWindow): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def getAcMechsState: IO[AcMechsState] =
      AcMechsState(AcLens.Ac.some, AcNdFilter.Open.some, AcFilter.Neutral.some).pure[IO]

    override def pwfs1Filter(filter: PwfsFilter): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs1FieldStop(fieldStop: PwfsFieldStop): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2Filter(filter: PwfsFilter): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2FieldStop(fieldStop: PwfsFieldStop): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def getPwfs1MechsState: IO[PwfsMechsState] =
      PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some).pure[IO]

    override def getPwfs2MechsState: IO[PwfsMechsState] =
      PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some).pure[IO]

    override def getBafflesState: IO[BafflesState] =
      BafflesState(CentralBafflePosition.Open, DeployableBafflePosition.Visible).pure[IO]

    override def pwfs1CircularBuffer(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def pwfs2CircularBuffer(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def oiwfsCircularBuffer(enable: Boolean): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]

    override def getPwfs1Configuration: IO[WfsConfiguration] = WfsConfiguration.default.pure[IO]

    override def getPwfs1ConfigurationStream: IO[Stream[IO, WfsConfiguration]] =
      (Stream.emit(WfsConfiguration.default) ++ Stream.never[IO]).pure[IO]

    override def getPwfs2Configuration: IO[WfsConfiguration] = WfsConfiguration.default.pure[IO]

    override def getPwfs2ConfigurationStream: IO[Stream[IO, WfsConfiguration]] =
      (Stream.emit(WfsConfiguration.default) ++ Stream.never[IO]).pure[IO]

    override def getOiwfsConfiguration: IO[WfsConfiguration] = WfsConfiguration.default.pure[IO]

    override def getOiwfsConfigurationStream: IO[Stream[IO, WfsConfiguration]] =
      (Stream.emit(WfsConfiguration.default) ++ Stream.never[IO]).pure[IO]

    override def refreshEphemerides(date: Option[LocalDate]): IO[CommandResult] =
      CommandResult.CommandSuccess.pure[IO]
  }

  def buildServer: IO[NavigateEngine[IO]] = for {
    r <- Ref.of[IO, GuideState](GuideState.default)
    p <- Ref.of[IO, TelescopeState](TelescopeState.default)
    q <- Ref.of[IO, GuidersQualityValues](GuidersQualityValues.default)
    g <- Ref.of[IO, GuideConfig](GuideConfig.defaultGuideConfig)
    u <- Ref.of[IO, AcMechsState](
           AcMechsState(AcLens.Ac.some, AcNdFilter.Open.some, AcFilter.Neutral.some)
         )
    v <-
      Ref.of[IO, PwfsMechsState](PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    w <-
      Ref.of[IO, PwfsMechsState](PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    c <- SignallingRef[IO].of(AllWfsConfiguration.default)
  } yield new NavigateEngineTest(
    new TcsSouthControllerSim[IO](r, p, u, v, w, c),
    new TcsNorthControllerSim[IO](r, p, u, v, w, c),
    g
  )

  def buildBadServer: IO[NavigateEngine[IO]] = for {
    r <- Ref.of[IO, GuideState](GuideState.default)
    p <- Ref.of[IO, TelescopeState](TelescopeState.default)
    g <- Ref.of[IO, GuideConfig](GuideConfig.defaultGuideConfig)
    u <- Ref.of[IO, AcMechsState](
           AcMechsState(AcLens.Ac.some, AcNdFilter.Open.some, AcFilter.Neutral.some)
         )
    v <-
      Ref.of[IO, PwfsMechsState](PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    w <-
      Ref.of[IO, PwfsMechsState](PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    c <- SignallingRef[IO].of(AllWfsConfiguration.default)
  } yield new NavigateEngineTest(
    new TcsSouthControllerSim[IO](r, p, u, v, w, c),
    new TcsNorthControllerSim[IO](r, p, u, v, w, c),
    g
  ) {
    override def oiwfsPark: IO[CommandResult] = CommandResult.CommandFailure("Error").pure[IO]
  }

  def buildServerUndefinedPos: IO[NavigateEngine[IO]] = for {
    r <- Ref.of[IO, GuideState](GuideState.default)
    p <- Ref.of[IO, TelescopeState](TelescopeState.default)
    g <- Ref.of[IO, GuideConfig](GuideConfig.defaultGuideConfig)
    u <- Ref.of[IO, AcMechsState](
           AcMechsState(AcLens.Ac.some, AcNdFilter.Open.some, AcFilter.Neutral.some)
         )
    v <-
      Ref.of[IO, PwfsMechsState](PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    w <-
      Ref.of[IO, PwfsMechsState](PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    c <- SignallingRef[IO].of(AllWfsConfiguration.default)
  } yield new NavigateEngineTest(
    new TcsSouthControllerSim[IO](r, p, u, v, w, c),
    new TcsNorthControllerSim[IO](r, p, u, v, w, c),
    g
  ) {
    override def getAcMechsState: IO[AcMechsState] = AcMechsState(none, none, none).pure[IO]

    override def getPwfs1MechsState: IO[PwfsMechsState] = PwfsMechsState(none, none).pure[IO]

    override def getPwfs2MechsState: IO[PwfsMechsState] = PwfsMechsState(none, none).pure[IO]

    override def getBafflesState: IO[BafflesState] =
      BafflesState(CentralBafflePosition.Open, DeployableBafflePosition.Visible).pure[IO]
  }

  def buildMapping(
    config: NavigateConfiguration = NavigateConfiguration.default,
    engIO:  IO[NavigateEngine[IO]] = buildServer
  ): IO[NavigateMappings[IO]] = for {
    eng <- engIO
    log <- Topic[IO, ILoggingEvent]
    gd  <- Topic[IO, GuideState]
    gq  <- Topic[IO, GuidersQualityValues]
    ts  <- Topic[IO, TelescopeState]
    aa  <- Topic[IO, AcquisitionAdjustment]
    lb  <- Ref.empty[IO, Seq[ILoggingEvent]]
    tot <- Topic[IO, TargetOffsets]
    ot  <- Topic[IO, FocalPlaneOffset]
    pt  <- Topic[IO, PointingCorrections]
    ac  <- Topic[IO, AcMechsState]
    p1  <- Topic[IO, PwfsMechsState]
    p2  <- Topic[IO, PwfsMechsState]
    mp  <- NavigateMappings[IO](config, eng, log, gd, gq, ts, aa, tot, ot, pt, ac, p1, p2, lb)
  } yield mp

  given Decoder[OperationOutcome] = Decoder.instance(h =>
    h.downField("result")
      .as[OperationResult]
      .map(r => OperationOutcome(r, h.downField("msg").as[String].toOption))
  )

  case class SimpleLoggingEvent(
    timestamp: Timestamp,
    level:     Level,
    thread:    String,
    message:   String
  ) extends ILoggingEvent {

    override def getThreadName: String = thread

    override def getLevel: Level = level

    override def getMessage: String = message

    override def getArgumentArray: Array[AnyRef] = Array.empty

    override def getFormattedMessage: String = message

    override def getLoggerName: String = ""

    override def getLoggerContextVO: LoggerContextVO = new LoggerContextVO("", Map.empty.asJava, 0)

    override def getThrowableProxy: IThrowableProxy = null

    override def getCallerData: Array[StackTraceElement] = Array.empty

    override def hasCallerData: Boolean = false

    override def getMarkerList: util.List[Marker] = List.empty.asJava

    override def getMDCPropertyMap: util.Map[String, String] = Map.empty.asJava

    override def getMdc: util.Map[String, String] = Map.empty.asJava

    override def getTimeStamp: Long = timestamp.toEpochMilli

    override def getNanoseconds: Int = 0

    override def getSequenceNumber: Long = 0

    override def getKeyValuePairs: util.List[KeyValuePair] = List.empty.asJava

    override def prepareForDeferredProcessing(): Unit = ()
  }

  given Decoder[SimpleLoggingEvent] = h =>
    for {
      ts <- h.downField("timestamp").as[Timestamp]
      l  <- h.downField("level").as[String].map(Level.toLevel)
      th <- h.downField("thread").as[String]
      ms <- h.downField("message").as[String]
    } yield SimpleLoggingEvent(ts, l, th, ms)

  given Decoder[ProbeGuide] = h =>
    for {
      from <- h.downField("from").as[GuideProbe]
      to   <- h.downField("to").as[GuideProbe]
    } yield ProbeGuide(from, to)

  given Decoder[GuideState] = h =>
    h.downField("mountOffload").as[Boolean].map { mnt =>
      val m2 = h
        .downField("m2Inputs")
        .as[List[String]]
        .toOption
        .map(_.flatMap(x => Enumerated[TipTiltSource].fromTag(x.toLowerCase.capitalize)))
      val m1 = h
        .downField("m1Input")
        .as[String]
        .toOption
        .flatMap(x => Enumerated[M1Source].fromTag(x.toLowerCase.capitalize))
      val cm = h.downField("m2Coma").as[Boolean].toOption
      val p1 = h.downField("p1Integrating").as[Boolean].toOption.exists(identity)
      val p2 = h.downField("p2Integrating").as[Boolean].toOption.exists(identity)
      val oi = h.downField("oiIntegrating").as[Boolean].toOption.exists(identity)
      val ac = h.downField("acIntegrating").as[Boolean].toOption.exists(identity)
      val pg = h.downField("probeGuide").as[ProbeGuide].toOption

      GuideState(
        MountGuideOption(mnt),
        m1.map(M1GuideConfig.M1GuideOn.apply).getOrElse(M1GuideConfig.M1GuideOff),
        m2.map(l =>
          if (l.isEmpty) M2GuideConfig.M2GuideOff
          else M2GuideConfig.M2GuideOn(ComaOption(cm.exists(identity)), l.toSet)
        ).getOrElse(M2GuideConfig.M2GuideOff),
        pg,
        p1,
        p2,
        oi,
        ac
      )
    }

  given Decoder[MechSystemState] = h =>
    for {
      prk <- h.downField("parked").as[ParkStatus]
      flw <- h.downField("follow").as[FollowStatus]
    } yield MechSystemState(prk, flw)

  given Decoder[TelescopeState] = h =>
    for {
      mnt  <- h.downField("mount").as[MechSystemState]
      scs  <- h.downField("scs").as[MechSystemState]
      crcs <- h.downField("crcs").as[MechSystemState]
      p1   <- h.downField("pwfs1").as[MechSystemState]
      p2   <- h.downField("pwfs2").as[MechSystemState]
      oi   <- h.downField("oiwfs").as[MechSystemState]
    } yield TelescopeState(
      mnt,
      scs,
      crcs,
      p1,
      p2,
      oi
    )

  given Decoder[NavigateState] = h =>
    for {
      swp <- h.downField("onSwappedTarget").as[Boolean]
    } yield NavigateState(swp)

  given Decoder[AcquisitionAdjustment] = h =>
    for {
      offset <- h.downField("offset").as[Offset]
      ipa    <- h.downField("ipa").as[Option[Angle]]
      iaa    <- h.downField("iaa").as[Option[Angle]]
      cmd    <- h.downField("command").as[AcquisitionAdjustmentCommand]
    } yield AcquisitionAdjustment(offset = offset, ipa = ipa, iaa = iaa, command = cmd)

  given Decoder[FocalPlaneOffset] = h =>
    for {
      deltaX <- h.downField("deltaX").as[Angle]
      deltaY <- h.downField("deltaY").as[Angle]
    } yield FocalPlaneOffset(FocalPlaneOffset.DeltaX(deltaX), FocalPlaneOffset.DeltaY(deltaY))

  given Decoder[TargetOffsets] = h =>
    for {
      sourceA <- h.downField("sourceA").as[FocalPlaneOffset]
      pwfs1   <- h.downField("pwfs1").as[FocalPlaneOffset]
      pwfs2   <- h.downField("pwfs2").as[FocalPlaneOffset]
      oiwfs   <- h.downField("oiwfs").as[FocalPlaneOffset]
    } yield TargetOffsets(sourceA, pwfs1, pwfs2, oiwfs)

  given Decoder[HorizontalAdjustment] = h =>
    for {
      az <- h.downField("azimuth").as[Angle]
      el <- h.downField("elevation").as[Angle]
    } yield HorizontalAdjustment(az, el)

  given Decoder[PointingCorrections] = h =>
    for {
      local <- h.downField("local").as[HorizontalAdjustment]
      guide <- h.downField("guide").as[HorizontalAdjustment]
    } yield PointingCorrections(local, guide)

  given Decoder[ServerConfiguration] = h =>
    for {
      version <- h.downField("version").as[String]
      site    <- h.downField("site").as[Site]
      odbUrl  <- h.downField("odbUri").as[String]
      ssoUrl  <- h.downField("ssoUri").as[String]
    } yield ServerConfiguration(version, site, odbUrl, ssoUrl)

  given Decoder[GuidersQualityValues.GuiderQuality] = h =>
    for {
      flux             <- h.downField("flux").as[Int]
      centroidDetected <- h.downField("centroidDetected").as[Boolean]
    } yield GuidersQualityValues.GuiderQuality(flux, centroidDetected)

  given Decoder[GuidersQualityValues] = h =>
    for {
      pwfs1 <- h.downField("pwfs1").as[GuidersQualityValues.GuiderQuality]
      pwfs2 <- h.downField("pwfs2").as[GuidersQualityValues.GuiderQuality]
      oiwfs <- h.downField("oiwfs").as[GuidersQualityValues.GuiderQuality]
    } yield GuidersQualityValues(pwfs1, pwfs2, oiwfs)

  given Decoder[AcMechsState] = h =>
    AcMechsState(h.downField("lens").as[AcLens].toOption,
                 h.downField("ndFilter").as[AcNdFilter].toOption,
                 h.downField("filter").as[AcFilter].toOption
    ).asRight

  given Decoder[PwfsMechsState] = h =>
    PwfsMechsState(h.downField("filter").as[PwfsFilter].toOption,
                   h.downField("fieldStop").as[PwfsFieldStop].toOption
    ).asRight

  given Decoder[BafflesState] = h =>
    for {
      cb <- h.downField("centralBaffle").as[CentralBafflePosition]
      db <- h.downField("deployableBaffle").as[DeployableBafflePosition]
    } yield BafflesState(cb, db)

}
