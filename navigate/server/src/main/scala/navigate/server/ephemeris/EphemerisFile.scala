// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.ephemeris

import cats.Eq
import cats.derived.*
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.HourAngle.*
import lucuma.core.math.JulianDate
import lucuma.core.model.Ephemeris
import lucuma.core.util.Timestamp
import mouse.boolean.*

import java.nio.charset.StandardCharsets
import java.text.DecimalFormat
import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.temporal.TemporalQuery
import java.util.Locale.US
import scala.util.matching.Regex

/** Support for formatting and parsing TCS ephemeris files. */
case class EphemerisFile(fileName: Path, ephemeris: Ephemeris.Key, start: Timestamp, end: Timestamp)
    derives Eq

object EphemerisFile {

  /** Header required by the TCS. */
  val Header: String =
    """****************************************************************************************
      | Date__(UT)__HR:MN Date_________JDUT     R.A.___(ICRF/J2000.0)___DEC  dRA*cosD d(DEC)/dt
      |****************************************************************************************""".stripMargin

  /** Marker for the start of ephemeris elements in the file. */
  val SOE: String = "$$SOE"

  /** Marker for the end of ephemeris elements in the file. */
  val EOE: String = "$$EOE"

  object Time {
    // Text date format
    val DateFormatString = "yyyy-MMM-dd HH:mm"

    // Regular expression that extracts the time string in the `DateFormatString`
    // format from a line in an ephemeris file.
    val TimeRegex: Regex = """(\d{4}-[a-zA-Z]{3}-\d{1,2}\s+\d{1,2}:\d{1,2})""".r

    val DateFormat: DateTimeFormatter =
      DateTimeFormatter.ofPattern(DateFormatString, US).withZone(UTC)

    def format(time: Instant): String =
      DateFormat.format(time)

    def parse(s: String): Either[Throwable, Instant] =
      Either.catchNonFatal {
        DateFormat.parse(s,
                         new TemporalQuery[Instant]() {
                           override def queryFrom(temporal: TemporalAccessor): Instant =
                             Instant.from(temporal)
                         }
        )
      }
  }

  case class Sexagesimal(
    degrees: Int,
    minutes: Int,
    seconds: Double
  )

  def format3(a: Int, b: Int, c: Double, max: Int, sep: String, fractionalDigits: Int): String = {
    val df =
      if (fractionalDigits > 0) new DecimalFormat(s"00.${"0" * fractionalDigits}")
      else new DecimalFormat("00")

    val s0          = df.format(c)
    val (s, carryC) = s0.startsWith("60").fold((df.format(0), 1), (s0, 0))

    val m0          = b + carryC
    val (m, carryB) = (m0 === 60).fold(("00", 1), (f"$m0%02d", 0))

    val x = a + carryB

    if (x === max) s"00${sep}00$sep${df.format(0)}"
    else f"$x%02d$sep$m$sep$s"
  }

  extension (a: Angle) {
    def toSexagesimal: Sexagesimal = {
      val m = (a.toDoubleDegrees - a.toDoubleDegrees.intValue) * 60
      val s = (m - m.intValue) * 60
      Sexagesimal(
        degrees = a.toDoubleDegrees.intValue,
        minutes = m.intValue,
        seconds = s
      )
    }
  }

  def formatSexigesimal(a: Angle, sep: String = ":", fractionalDigits: Int = 2): String = {
    val dms = a.toSexagesimal
    format3(dms.degrees, dms.minutes, dms.seconds, 360, sep, fractionalDigits)
  }

  def formatDMS(dec: Declination, sep: String = ":", fractionalDigits: Int = 2): String = {
    val a0       = Angle.toSignedDoubleDegrees(dec.toAngle)
    val (a, sgn) = if (a0 < 0) (a0.abs, "-") else (a0, "")
    s"$sgn${formatSexigesimal(Angle.fromDoubleDegrees(a), sep, fractionalDigits)}"
  }

  def formatHMS(hms: HMS, sep: String = ":", fractionalDigits: Int = 3): String =
    format3(
      hms.hours,
      hms.minutes,
      hms.seconds.toDouble + hms.milliseconds.toDouble / 1000.0 + hms.microseconds.toDouble / 1e6,
      24,
      sep,
      fractionalDigits
    )

  // TODO: We need to know which site we need inside the Ephemeris.
  def format(ephemeris: Ephemeris.Horizons, site: Site): String = {
    def formatCoords(coords: Coordinates): String = {
      val ra  = formatHMS(HourAngle.hms.get(coords.ra.toHourAngle), " ", 4)
      val dec = formatDMS(coords.dec, " ", 3)
      // Add spacing as required for TCS.
      f"$ra%14s $dec%13s"
    }

    val lines = (if (site === Site.GS) ephemeris.elements.gs else ephemeris.elements.gn).map {
      entry =>
        val timeS     = Time.format(entry.when)
        val jdS       = f"${JulianDate.ofInstant(entry.when).toDouble}%.9f"
        val coordsS   = formatCoords(entry.coordinates)
        val raTrackS  =
          f"${Angle.signedDecimalArcseconds.get(entry.velocity.p.toAngle).toDouble}%9.5f"
        val decTrackS =
          f"${Angle.signedDecimalArcseconds.get(entry.velocity.q.toAngle).toDouble}%9.5f"
        s" $timeS $jdS    $coordsS $raTrackS $decTrackS"
    }

    lines.mkString(s"$Header\n$SOE\n", "\n", lines.isEmpty.fold("", "\n") + s"$EOE\n")
  }

  object EphemerisParser {

    def parseEntries[F[_]: Async](fl: Path): F[List[Timestamp]] =
      Files.forAsync
        .readAll(fl)
        .through(fs2.text.utf8.decode)
        .through(fs2.text.lines)
        .dropThrough(!_.contains(SOE))
        .takeWhile(!_.contains(EOE))
        .map(parseInstant)
        .flattenOption
        .compile
        .toList

    def parseInstant(str: String): Option[Timestamp] = {
      val a = str.trim.split("\\s+").toList.take(2).mkString(" ")
      val v = Time.parse(a)

      v.toOption.flatMap(Timestamp.fromInstant)
    }

    def parseElement(fl: Path): Option[Ephemeris.Key] = Ephemeris.Key.fromString.getOption(
      java.net.URLDecoder.decode(fl.fileName.toString.takeWhile(_ =!= '.'),
                                 StandardCharsets.UTF_8.toString
      )
    )

    def parse[F[_]: Async](fl: Path): F[Option[EphemerisFile]] = parseEntries(fl).map { l =>
      for {
        key <- parseElement(fl)
        st  <- l.headOption
        end <- l.lastOption
      } yield EphemerisFile(fl, key, st, end)
    }

  }

  def filenameFromKey(key: Ephemeris.Key): String =
    java.net.URLEncoder.encode(s"${key.keyType}_${key.des}.eph", StandardCharsets.UTF_8.toString)

}
