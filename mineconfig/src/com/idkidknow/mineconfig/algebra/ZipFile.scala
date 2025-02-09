package com.idkidknow.mineconfig.algebra

import cats.data.OptionT
import cats.effect.kernel.Async
import cats.effect.kernel.Concurrent
import cats.effect.kernel.Resource
import fs2.Stream
import fs2.io.file.Path

import java.util.zip.ZipFile as JZipFile

trait ZipFile[F[_]] {
  def readEntry(name: String): F[Option[Stream[F, Byte]]]

  def readEntryAsString(
      name: String
  )(using Concurrent[F]): F[Option[String]] = {
    OptionT(readEntry(name)).semiflatMap(
      _.through(fs2.text.utf8.decode)
        .compile.string
    ).value
  }
}

object ZipFile {
  trait Read[F[_]] {
    def read(zip: Path): Resource[F, ZipFile[F]]
  }

  object Read {
    def apply[F[_]: Read as inst]: Read[F] = inst

    def fromAsync[F[_]: Async as F]: Read[F] = (zip: Path) =>
      Resource.make(
        F.blocking(new JZipFile(zip.toNioPath.toFile))
      )(zipFile => F.blocking(zipFile.close()))
        .map { jZipFile => entryName =>
          OptionT(F.delay(Option(jZipFile.getEntry(entryName)))).map { entry =>
            fs2.io.readInputStream(
              F.delay(jZipFile.getInputStream(entry)),
              8192,
            )
          }.value
        }
  }
}
