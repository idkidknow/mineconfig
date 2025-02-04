package com.idkidknow.mineconfig.vanilla

import cats.syntax.all.*
import com.idkidknow.mineconfig.utils.given
import io.circe.Decoder
import org.http4s.Uri

/** An entry of `libraries` in [[https://minecraft.wiki/w/Client.json]]
 *
 *  For 1.12.2+ only currently
 *
 *  TODO: support legacy versions (may have "${arch}" in classifier,
 *  mainArtifact may be optional)
 */
final case class LibraryDescription(
    mainArtifact: LibraryDescription.ArtifactInfo,
    otherArtifacts: Map[String, LibraryDescription.ArtifactInfo],
    mavenName: String,
    natives: Map[String, String],
    nativeExtractExclude: Set[String],
    rules: List[Rule],
) {
  def allArtifacts: List[ArtifactDescription] = {
    def infoToDesc(info: LibraryDescription.ArtifactInfo): ArtifactDescription =
      ArtifactDescription(
        mavenName = mavenName,
        rules = rules,
        path = info.path,
        sha1 = info.sha1,
        url = info.url,
        nativeInfo = None,
      )
    val main = infoToDesc(mainArtifact)
    val nativeArtifacts = natives.toList.flatMap { case (osName, classifier) =>
      val nativeInfo = ArtifactDescription.NativeInfo(
        osName = osName,
        classifier = classifier,
        extractExclude = nativeExtractExclude,
      )
      otherArtifacts.get(classifier) match {
        case None => List.empty
        case Some(info) =>
          List(infoToDesc(info).copy(nativeInfo = Some(nativeInfo)))
      }
    }
    List(main) ++ nativeArtifacts
  }
}

object LibraryDescription {
  final case class ArtifactInfo(
      path: String,
      sha1: String,
      url: Uri,
  )
  object ArtifactInfo {
    given Decoder[ArtifactInfo] = Decoder.derived
  }

  private final case class Downloads(
      artifact: ArtifactInfo,
      classifiers: Option[Map[String, ArtifactInfo]],
  )
  private final case class Extract(
      exclude: Set[String]
  )
  private final case class Library(
      downloads: Downloads,
      name: String,
      natives: Option[Map[String, String]],
      extract: Option[Extract],
      rules: Option[List[Rule]],
  )
  given Decoder[LibraryDescription] = {
    given Decoder[Downloads] = Decoder.derived
    given Decoder[Extract] = Decoder.derived
    Decoder.derived[Library].map { lib =>
      LibraryDescription(
        mainArtifact = lib.downloads.artifact,
        otherArtifacts = lib.downloads.classifiers.getOrElse(Map.empty),
        mavenName = lib.name,
        natives = lib.natives.getOrElse(Map.empty),
        nativeExtractExclude = lib.extract.map(_.exclude).getOrElse(Set.empty),
        rules = lib.rules.getOrElse(List.empty),
      )
    }
  }
}
