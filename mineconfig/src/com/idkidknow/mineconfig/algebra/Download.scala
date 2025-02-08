package com.idkidknow.mineconfig.algebra

import cats.MonadThrow
import cats.effect.kernel.Async
import cats.effect.std.Console
import cats.effect.std.Semaphore
import cats.syntax.all.*
import fs2.Stream
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import fs2.io.file.Files
import fs2.io.file.Path
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client

trait Download[F[_]] {
  def downloadFile(url: Uri, dest: Path): F[Unit]

  def downloadFileWithSHA1(url: Uri, dest: Path, sha1: String): F[Unit]

  def downloadFile(url: String, dest: Path)(using F: MonadThrow[F]): F[Unit] =
    for {
      uri <- F.pure(Uri.fromString(url)).rethrow
      _ <- downloadFile(uri, dest)
    } yield ()
}

object Download {
  def apply[F[_]: Download as inst]: Download[F] = inst

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def fromHttp4s[F[_]: Async](
      client: Client[F],
      semaphore: Semaphore[F],
  ): Download[F] =
    new Download[F] {
      override def downloadFile(url: Uri, dest: Path): F[Unit] = {
        val request = Request[F](method = Method.GET, uri = url)
        val s: Stream[F, Byte] = client.stream(request).flatMap { response =>
          //      val fileSize: List[Long] = response.headers.get(CIString("Content-Length"))
          //        .toList.flatMap(_.toList).map(_.value.toLong)
          //      assert(fileSize.length <= 1)
          //      val size: Long = fileSize.headOption.getOrElse(0)
          response.body
        }
        Console.make[F].println(show"Downloading $url to $dest") >>
          semaphore.acquire >>
          dest.parent.map(Files[F].createDirectories(_)).sequence.void >>
          s.through(Files[F].writeAll(dest)).compile.drain >>
          semaphore.release
      }

      override def downloadFileWithSHA1(
          url: Uri,
          dest: Path,
          sha1: String,
      ): F[Unit] = {
        val checkSHA1: F[Boolean] = {
          given Hashing[F] = Hashing.forSync
          Files[F].readAll(dest)
            .through(
              Hashing[F].hash(HashAlgorithm.SHA1)
            ).compile.onlyOrError.map(_.toString == sha1)
        }

        val downloadAndCheck: F[Unit] = {
          downloadFile(url, dest) >>
            checkSHA1.ifM(
              ifTrue = ().pure[F],
              ifFalse = MonadThrow[F].raiseError(
                new RuntimeException(show"failed to download $url")
              ),
            )
        }

        Files[F].exists(dest).ifM(
          ifTrue = checkSHA1.ifM(
            ifTrue = ().pure[F],
            ifFalse = downloadAndCheck,
          ),
          ifFalse = downloadAndCheck,
        )
      }
    }
}
