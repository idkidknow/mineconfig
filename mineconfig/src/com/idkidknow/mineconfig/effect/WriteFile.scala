package com.idkidknow.mineconfig.effect

import cats.syntax.all.*
import cats.effect.kernel.Async
import fs2.io.file.{Files, Path}

trait WriteFile[F[_]] {
  def writeString(path: Path, content: String): F[Unit]
}

object WriteFile {
  def apply[F[_]: WriteFile as inst]: WriteFile[F] = inst

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def fromAsync[F[_]: Async]: WriteFile[F] = (path, content) => {
    path.parent.map(Files[F].createDirectories(_)).sequence.void >>
      fs2.Stream(content).through(Files[F].writeUtf8(path)).compile.drain
  }
}
