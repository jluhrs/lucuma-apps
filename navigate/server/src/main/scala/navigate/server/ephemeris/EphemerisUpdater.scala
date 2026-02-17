// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.ephemeris

import cats.MonadThrow
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.core.enums.Site
import lucuma.core.model.Ephemeris
import lucuma.core.model.LocalObservingNight
import lucuma.core.util.DateInterval
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.horizons.HorizonsClient
import navigate.server.OdbProxy
import org.typelevel.log4cats.Logger

import java.time.LocalDate
import java.time.ZoneOffset

trait EphemerisUpdater[F[_]] {
  def refreshEphemerides(dateInterval: DateInterval): F[Unit]
}

object EphemerisUpdater {

  case class EphemerisTimeRange(ephemeris: Ephemeris.Key, start: Timestamp, end: Timestamp)

  private def createEphemerisFiles[F[_]: Async](
    horizonsClient: HorizonsClient[F],
    targets:        List[Ephemeris.Key],
    dateInterval:   DateInterval,
    site:           Site,
    path:           Path
  ): F[List[String]] = targets
    .map {
      case h: Ephemeris.Key.Horizons =>
        (for {
          st  <- dateInterval.start.localNightBoundary(site)
          end <- dateInterval.end.localNightBoundary(site)
        } yield horizonsClient
          .ephemeris(h,
                     st.toInstant,
                     end.toInstant,
                     TimeSpan.between(st, end).toMinutes.toInt + 1,
                     HorizonsClient.SiteOption.Both
          )
          .flatMap(
            _.fold(
              _.some.pure[F],
              x =>
                Files.forAsync
                  .writeUtf8(path / EphemerisFile.filenameFromKey(h))(
                    Stream.emits(List(EphemerisFile.format(x, site)))
                  )
                  .compile
                  .drain
                  .as(none)
            )
          )).getOrElse(none.pure[F])
      case _                         => none.pure[F]
    }
    .sequence
    .map(_.flattenOption)

  private[ephemeris] def readEphemerisFiles[F[_]: Async](
    filePath: Path
  ): F[List[EphemerisFile]] = Files
    .forAsync[F]
    .list(filePath)
    .filter(_.extName === ".eph")
    .evalMap(EphemerisFile.EphemerisParser.parse)
    .compile
    .toList
    .map(_.flattenOption)

  extension (date: LocalDate) {
    private def localNightBoundary(site: Site): Option[Timestamp] =
      Timestamp.fromLocalDateTime(
        LocalObservingNight.localDate
          .reverseGet(date)
          .start
          .atZone(site.place.timezone)
          .withZoneSameInstant(ZoneOffset.UTC)
          .toLocalDateTime
      )
  }

  private def isCovered(
    target:       Ephemeris.Key,
    dateInterval: DateInterval,
    file:         EphemerisFile,
    site:         Site
  ): Boolean = target === file.ephemeris && dateInterval.start
    .localNightBoundary(site)
    .exists(
      _ >= file.start
    ) && dateInterval.end.localNightBoundary(site).exists(_ <= file.end)

  private def threshFilesAndTargets[F[_]](
    targets:      List[Ephemeris.Key],
    dateInterval: DateInterval,
    files:        List[EphemerisFile],
    site:         Site
  ): (List[Ephemeris.Key], List[EphemerisFile]) = (
    targets.filter(f => !files.exists(isCovered(f, dateInterval, _, site))),
    List.empty
  )

  private def deleteFiles[F[_]: Async](l: List[EphemerisFile]): F[Unit] =
    l.map(x => Files.forAsync.delete(x.fileName)).sequence.void

  def build[F[_]: {MonadThrow, Async}](
    site:           Site,
    filePath:       Path,
    odbProxy:       OdbProxy[F],
    horizonsClient: HorizonsClient[F]
  )(using
    L:              Logger[F]
  ): EphemerisUpdater[F] = new EphemerisUpdater[F] {
    /*
     * The process to refresh the ephemeris files in TCS must:
     * Collect all the non sidereal targets used by active observations for a given time interval (usually 24 hours)
     * Read the ephemeris files available to TCS and collect information of non sidereal target ids and time range
     * Split the ephemeris files between the ones that covered the required targets and the ones that don't. Delete the ones that cover only the past.
     * Split non sidereal targets between the ones covered by the existing files and the ones that are not covered.
     * For the ones that are not covered, read ephemeris data from Horizons, format and save them.
     * If files cannot be created, send notification emails.
     */
    override def refreshEphemerides(dateInterval: DateInterval): F[Unit] = for {
      files             <- readEphemerisFiles(filePath)
      targets           <- odbProxy.queryNonSiderealObs(site, dateInterval.start, dateInterval.end)
      _                 <- L.info(s"Refresh ephemerides: Targets needed: ${targets.mkString(", ")}")
      (toLoad, toDelete) = threshFilesAndTargets(targets, dateInterval, files, site)
      _                 <- L.info(s"Refresh ephemerides: Files to be generated for ${toLoad.mkString(", ")}")
      _                 <- deleteFiles(toDelete)
      errors            <- createEphemerisFiles(horizonsClient, toLoad, dateInterval, site, filePath)
      _                 <- L.info(s"Refresh ephemerides: Errors when generating files: ${errors.mkString(", ")}")
                             .whenA(errors.nonEmpty)
    } yield ()

  }
}
