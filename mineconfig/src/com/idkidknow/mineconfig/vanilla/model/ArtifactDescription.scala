package com.idkidknow.mineconfig.vanilla.model

import cats.Monad
import cats.syntax.all.*
import com.idkidknow.mineconfig.algebra.Download
import fs2.io.file.Path
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import org.http4s.Uri

final case class ArtifactDescription(
    mavenName: String,
    rules: List[Rule],
    path: String,
    sha1: String,
    url: Uri,
    nativeInfo: Option[ArtifactDescription.NativeInfo],
) {
  def check(osInfo: OSInfo): Boolean = {
    rules.forall(_.check(Map.empty, osInfo))
    && nativeInfo.forall(_.osName == osInfo.name)
  }

  def download[F[_]: {Monad, Download}](
      dir: Path
  ): F[ArtifactDescription.Local] = {
    val filepath = dir.resolve(path)
    Download[F].downloadFileWithSHA1(url, filepath, sha1)
      >> ArtifactDescription.Local(mavenName, filepath, nativeInfo).pure[F]
  }
}

object ArtifactDescription {
  final case class NativeInfo(
      osName: String,
      classifier: String,
      extractExclude: Set[String],
  )

  final case class Local(
      mavenName: String,
      path: Path,
      nativeInfo: Option[NativeInfo],
  )
  object Local {
    given Codec[Local] = {
      given Codec[Path] = Codec.from(
        Decoder.decodeString.map(Path(_)),
        Encoder.encodeString.contramap(_.toNioPath.toString),
      )
      given Codec[NativeInfo] = Codec.derived
      Codec.derived
    }
  }
}
