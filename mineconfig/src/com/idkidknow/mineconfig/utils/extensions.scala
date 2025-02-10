package com.idkidknow.mineconfig.utils

import cats.Applicative
import cats.MonadThrow
import cats.syntax.all.*
import fs2.Pipe
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path

extension [A](option: Option[A]) {
  def expect[F[_]: MonadThrow](msg: String): F[A] = {
    option.toRight(new RuntimeException(msg)).pure[F].rethrow
  }
}

extension [F[_]: Applicative](files: Files[F]) {

  /** Create parent folder recursively if not exists before writeAll */
  def writeAllR(path: Path): Pipe[F, Byte, Unit] = { stream =>
    Stream.exec(path.parent match {
      case Some(parent) => files.createDirectories(parent)
      case None => ().pure[F]
    }) ++
      stream.through(files.writeAll(path))
  }
  
  def writeUtf8R(path: Path): Pipe[F, String, Unit] = { stream =>
    stream.through(fs2.text.utf8.encode).through(writeAllR(path))
  }
}
