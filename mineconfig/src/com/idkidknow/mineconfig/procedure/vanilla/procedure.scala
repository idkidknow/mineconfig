package com.idkidknow.mineconfig.procedure.vanilla

import cats.MonadThrow
import cats.Parallel
import cats.syntax.all.*
import com.idkidknow.mineconfig.algebra.Download
import com.idkidknow.mineconfig.algebra.StringRW
import com.idkidknow.mineconfig.model.vanilla.ArtifactDescription
import com.idkidknow.mineconfig.model.vanilla.LibraryDescription
import com.idkidknow.mineconfig.model.vanilla.OSInfo
import com.idkidknow.mineconfig.utils.{*, given}
import fs2.io.file.Path
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.Printer
import io.circe.optics.JsonPath.root
import io.circe.syntax.*
import org.http4s.Uri

@SuppressWarnings(Array("org.wartremover.warts.Any"))
def downloadClientJson[F[_]: {MonadThrow as F, StringRW, Download}](
    versionManifest: Path,
    version: String,
    dest: Path,
): F[Unit] = {
  val manifestJson = StringRW[F].readAsJson(versionManifest)
  manifestJson.flatMap { json =>
    val targetUrl: Option[String] = for {
      versions <- root.versions.arr.getOption(json)
      target <- versions.find(obj =>
        root.id.string.getOption(obj).contains(version)
      )
      url <- root.url.string.getOption(target)
    } yield url
    targetUrl.expect("Failed to find client.json url").flatMap { url =>
      Download[F].downloadFile(url, dest)
    }
  }
}

def downloadLibraries[
    F[_]: {MonadThrow as F, Parallel, StringRW, Download}
](
    osInfo: OSInfo,
    clientJson: Path,
    outputDir: Path,
    outputArtifactInfos: Path,
): F[Unit] = {
  StringRW[F].readAsJson(clientJson).flatMap { json =>
    val artifacts = for {
      libraryJsons <- root.libraries.arr.getOption(json)
      libraries: Vector[LibraryDescription] <- libraryJsons.map { json =>
        Decoder[LibraryDescription].decodeJson(json).toOption
      }.sequence // None if one of the libraries is invalid
      artifacts = libraries.flatMap(_.allArtifacts)
        .filter(_.check(osInfo))
    } yield artifacts

    artifacts.expect("Failed to parse libraries").flatMap { artifacts =>
      for {
        downloaded <- artifacts.map(_.download(outputDir)).parSequence
        _ <- StringRW[F].writeString(
          outputArtifactInfos,
          Printer.spaces2.copy(dropNullValues = true)
            .print(downloaded.asJson),
        )
      } yield ()
    }
  }
}

def downloadGameOrMappings[
    F[_]: {MonadThrow as F, Parallel, StringRW, Download}
](
    clientJson: Path,
    artifactAndDest: Map[String, Path],
): F[Unit] = {
  def download(fileInfo: Json, path: Path): F[Unit] = {
    final case class FileInfo(sha1: String, url: Uri)
    for {
      info <- Decoder.derived[FileInfo].decodeJson(fileInfo).pure[F].rethrow
      _ <- Download[F].downloadFileWithSHA1(info.url, path, info.sha1)
    } yield ()
  }

  for {
    json <- StringRW[F].readAsJson(clientJson)
    downloads: Map[String, Json] <- root.downloads.obj.getOption(json).expect(
      "downloads field not found in client.json"
    ).map(_.toMap)
    _ <- artifactAndDest.toList.map({ case (name, path) =>
      downloads.get(name).toRight(
        new RuntimeException(
          show"Failed to find artifact \"$name\" in downloads"
        )
      ).pure[F].rethrow.flatMap { fileInfo =>
        download(fileInfo, path)
      }
    }).parSequence
  } yield ()
}
