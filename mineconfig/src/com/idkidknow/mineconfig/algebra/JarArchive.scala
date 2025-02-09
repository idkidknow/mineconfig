package com.idkidknow.mineconfig.algebra

import cats.effect.kernel.Async
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Pipe
import fs2.Stream

import java.io.BufferedInputStream
import java.util.jar.JarEntry
import java.util.jar.JarInputStream
import java.util.jar.JarOutputStream

trait JarArchive[F[_]] {
  def unarchive: Pipe[F, Byte, (JarEntry, Stream[F, Byte])]

  def archive: Pipe[F, (JarEntry, Stream[F, Byte]), Byte]
}

object JarArchive {
  def apply[F[_]: JarArchive as inst]: JarArchive[F] = inst

  def fromAsync[F[_]: Async as F]: JarArchive[F] = new JarArchive[F] {
    override def unarchive: Pipe[F, Byte, (JarEntry, Stream[F, Byte])] = in =>
      in.through(fs2.io.toInputStream)
        .map(new BufferedInputStream(_))
        .map(new JarInputStream(_))
        .flatMap { in =>
          val readNext = Resource.make(F.blocking(Option(in.getNextJarEntry))) {
            _ => F.blocking(in.closeEntry())
          }
          val lock = Stream.eval(Deferred[F, Unit])
          def entries: Stream[F, (JarEntry, Stream[F, Byte])] = {
            Stream.resource(readNext).flatMap {
              case Some(entry) =>
                lock.flatMap { lock =>
                  val currentStream =
                    fs2.io.readInputStream(F.delay(in), 8192, false) ++
                      Stream.exec(lock.complete(()).void)
                  Stream.emit((entry, currentStream)) ++
                    Stream.exec(lock.get.void)
                } ++ entries
              case None => Stream.empty
            }
          }
          entries
        }

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    override def archive: Pipe[F, (JarEntry, Stream[F, Byte]), Byte] =
      stream =>
        fs2.io.readOutputStream(8192) { out =>
          Resource.make(F.blocking(new JarOutputStream(out)))(s =>
            F.blocking(s.close())
          ).use { out =>
            stream.flatMap { case (entry, bytes) =>
              Stream.exec(F.blocking(out.putNextEntry(entry))) ++
                bytes.through(fs2.io.writeOutputStream(out.pure[F], false)) ++
                Stream.exec(F.blocking(out.closeEntry()))
            }.compile.drain
          }
        }
  }
}
