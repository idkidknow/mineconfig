package com.idkidknow.mineconfig.procedure

import cats.syntax.all.*
import cats.{MonadThrow, Parallel}
import com.idkidknow.mineconfig.effect.{Download, ReadFile, WriteFile}
import com.idkidknow.mineconfig.utils.given
import com.idkidknow.mineconfig.vanilla.{
  ArtifactDescription,
  LibraryDescription,
  OSInfo,
}
import fs2.io.file.Path
import io.circe.optics.JsonPath.root
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json, Printer}
import org.http4s.Uri

@SuppressWarnings(Array("org.wartremover.warts.Any"))
def downloadClientJson[F[_]: {MonadThrow as F, ReadFile, WriteFile, Download}](
    versionManifest: Path,
    version: String,
    dest: Path,
): F[Unit] = {
  val manifestJson = ReadFile[F].readAsJson(versionManifest)
  manifestJson.flatMap { json =>
    val targetUrl: Option[String] = for {
      versions <- root.versions.arr.getOption(json)
      target <- versions.find(obj =>
        root.id.string.getOption(obj).contains(version)
      )
      url <- root.url.string.getOption(target)
    } yield url
    targetUrl.fold(
      F.raiseError(new RuntimeException("Failed to find client.json url")).void
    ) { url =>
      Download[F].downloadFile(url, dest)
    }
  }
}

def downloadLibraries[
    F[_]: {MonadThrow as F, Parallel, ReadFile, WriteFile, Download}
](
    osInfo: OSInfo,
    clientJson: Path,
    outputDir: Path,
    outputArtifactInfos: Path,
): F[Unit] = {
  ReadFile[F].readAsJson(clientJson).flatMap { json =>
    val artifacts = for {
      libraryJsons <- root.libraries.arr.getOption(json)
      libraries: Vector[LibraryDescription] <- libraryJsons.map { json =>
        Decoder[LibraryDescription].decodeJson(json).toOption
      }.sequence // None if one of the libraries is invalid
      artifacts = libraries.flatMap(_.allArtifacts)
        .filter(_.check(osInfo))
    } yield artifacts

    artifacts.fold(
      F.raiseError(new RuntimeException("Failed to parse libraries"))
    ) { artifacts =>
      for {
        downloaded <- artifacts.map(_.download(outputDir)).parSequence
        _ <- WriteFile[F].writeString(
          outputArtifactInfos,
          Printer.spaces2.copy(dropNullValues = true)
            .print(downloaded.asJson),
        )
      } yield ()
    }
  }
}

def downloadGameOrMappings[
    F[_]: {MonadThrow as F, Parallel, ReadFile, WriteFile, Download}
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
    json <- ReadFile[F].readAsJson(clientJson)
    downloads: Map[String, Json] <- root.downloads.obj.getOption(json).toRight(
      new RuntimeException("downloads field not found in client.json")
    ).pure[F].rethrow.map(_.toMap)
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
