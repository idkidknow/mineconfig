package com.idkidknow.mineconfig.mcpconfig.procedure

import cats.effect.Concurrent
import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.syntax.all.*
import com.idkidknow.mineconfig.algebra.JarArchive
import com.idkidknow.mineconfig.algebra.MavenCache
import com.idkidknow.mineconfig.algebra.StringRW
import com.idkidknow.mineconfig.algebra.ZipFile
import com.idkidknow.mineconfig.mcpconfig.model.FunctionDescription
import com.idkidknow.mineconfig.utils.*
import com.idkidknow.mineconfig.vanilla.model.ArtifactDescription
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.Decoder
import io.circe.Json
import io.circe.Printer
import io.circe.optics.JsonPath.root
import io.circe.syntax.*

import java.util.jar.Manifest

private def readConfigJson[F[_]: Concurrent](zipFile: ZipFile[F]): F[Json] = {
  val str: F[String] = zipFile.readEntryAsString("config.json")
    .flatMap(_.expect("config.json not found"))
  str.map(io.circe.parser.parse).rethrow
}

/** Read `functions` field in `config.json` and downloads these functions.
 *
 *  Write a description file in the format
 *  `Map[String, FunctionDescription.Local]` (see [[FunctionDescription.Local]])
 *  which provides necessary information to execute the function.
 */
def downloadFunctions[
    F[_]: {Concurrent, ZipFile.Read, MavenCache, StringRW}
](
    mcpconfigZip: Path,
    outputDescription: Path,
): F[Unit] = for {
  json <- ZipFile.Read[F].read(mcpconfigZip).use(readConfigJson(_))
  javaTarget = root.java_target.int.getOption(json).getOrElse(8)
  functions: Map[String, Json] <-
    root.functions.obj.getOption(json).expect(
      "no functions in config.json"
    ).map(_.toMap)
  desc: Map[String, FunctionDescription] <-
    functions.toList.traverse { case (name, json) =>
      Decoder[FunctionDescription].decodeJson(json).map((name, _))
    }.pure[F].rethrow.map(_.toMap)
  local: Map[String, FunctionDescription.Local] <-
    desc.parUnorderedTraverse(_.cache(javaTarget))
  _ <- StringRW[F].writeString(outputDescription, local.asJson.spaces2)
} yield ()

/** Main part of `strip` function in mcpconfig */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
def strip[F[_]: {Concurrent, Files, JarArchive}](
    inputJar: Path,
    classList: List[String],
    whitelist: Boolean,
    output: Path,
): F[Unit] = Files[F].readAll(inputJar)
  .through(JarArchive[F].unarchive)
  .flatMap { case (entry, bytes) =>
    if (!entry.isDirectory && whitelist == classList.contains(entry.getName)) {
      Stream.emit((entry, bytes))
    } else {
      Stream.exec(bytes.compile.drain)
    }
  }
  .through(JarArchive[F].archive)
  .through(Files[F].writeAll(output))
  .compile.drain

/** Main part of `listLibraries` function in mcpconfig when the arg `bundle` is
 *  provided.
 *
 *  Extracts libraries from the bundle and writes a description file in format
 *  `List[ArtifactDescription.Local]`. (see [[ArtifactDescription.Local]])
 */
def listBundleLibraries[F[_]: {Async as F, ZipFile.Read, Files}](
    bundle: Path,
    outputLibrariesDir: Path,
    outputArtifactInfos: Path,
): F[Unit] = ZipFile.Read[F].read(bundle).use { zipFile =>
  val checkFormat: F[Unit] = for {
    optStream <- zipFile.readEntry("META-INF/MANIFEST.MF")
    manifestStream <- optStream.expect("MANIFEST.MF not found")
    manifest <- manifestStream.through(fs2.io.toInputStream)
      .map(new Manifest(_))
      .compile.onlyOrError
    optFormat = Option(manifest.getMainAttributes.getValue("Bundler-Format"))
    format <- optFormat.expect("Attribute \"Bundler-Format\" not found")
    _ <-
      if (format != "1.0")
        F.raiseError(new RuntimeException("Unsupported format"))
      else F.unit
  } yield ()

  for {
    _ <- checkFormat
    optStr <- zipFile.readEntryAsString("META-INF/libraries.list")
    librariesStr <- optStr.expect("libraries.list not found")
    lines = Stream(librariesStr).through(fs2.text.lines).toList
    libraries: List[(String, Path)] <- lines.flatMap { line =>
      line.split('\t') match {
        case Array(_, mavenName, entryName) =>
          List((mavenName, entryName))
        case _ => Nil
      }
    }.parTraverse { case (mavenName, entryName) =>
      val dest: Path = outputLibrariesDir.resolve(entryName)

      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val extract: F[Unit] =
        dest.parent.map(Files[F].createDirectories(_)).sequence.void >>
          Stream.eval(
            zipFile.readEntry(show"META-INF/libraries/$entryName")
              .flatMap(_.expect(show"Library $entryName not found"))
          ).flatten.through(Files[F].writeAll(dest))
            .compile.drain

      extract >> (mavenName, dest).pure[F]
    }
    desc = libraries.map { case (mavenName, path) =>
      ArtifactDescription.Local(mavenName, path, None)
    }
    _ <- StringRW[F].writeString(
      outputArtifactInfos,
      Printer.spaces2.copy(dropNullValues = true)
        .print(desc.asJson),
    )
  } yield ()
}
