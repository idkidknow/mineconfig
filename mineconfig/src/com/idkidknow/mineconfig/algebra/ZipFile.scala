package com.idkidknow.mineconfig.algebra

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import fs2.Stream
import fs2.io.file.Path

import java.util.zip.ZipFile as JZipFile

trait ZipFile[F[_]] {
  def readEntry(name: String): Stream[F, Byte]
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
          fs2.io.readInputStream(
            F.delay(jZipFile.getInputStream(jZipFile.getEntry(entryName))),
            8192,
          )
        }
  }
}
