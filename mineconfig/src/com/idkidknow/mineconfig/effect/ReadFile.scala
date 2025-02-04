package com.idkidknow.mineconfig.effect

import cats.MonadThrow
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.io.file.{Files, Path}
import io.circe.Json

trait ReadFile[F[_]] {
  def readAsString(path: Path): F[String]

  def readAsJson(path: Path)(using MonadThrow[F]): F[Json] = {
    readAsString(path).map(io.circe.parser.parse).rethrow
  }
}

object ReadFile {
  def apply[F[_]: ReadFile as inst]: ReadFile[F] = inst

  def fromAsync[F[_]: Async]: ReadFile[F] = (path: Path) =>
    Files[F].readUtf8(path).compile.string
}
