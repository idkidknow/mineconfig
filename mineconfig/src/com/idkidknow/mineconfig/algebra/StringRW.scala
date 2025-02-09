package com.idkidknow.mineconfig.algebra

import cats.MonadThrow
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.Json

trait StringRW[F[_]] {
  def readAsString(path: Path): F[String]

  def writeString(path: Path, content: String): F[Unit]

  def readAsJson(path: Path)(using MonadThrow[F]): F[Json] = {
    readAsString(path).map(io.circe.parser.parse).rethrow
  }
}

object StringRW {
  def apply[F[_]: StringRW as inst]: StringRW[F] = inst

  given fromConcurrentFiles[F[_]: {Concurrent, Files}]: StringRW[F] =
    new StringRW[F] {
      override def readAsString(path: Path): F[String] =
        Files[F].readUtf8(path).compile.string

      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      override def writeString(path: Path, content: String): F[Unit] =
        path.parent.map(Files[F].createDirectories(_)).sequence.void >>
          fs2.Stream(content).through(Files[F].writeUtf8(path)).compile.drain
    }

}
