package com.idkidknow.mineconfig.model.mcpconfig

import cats.Functor
import cats.syntax.all.*
import com.idkidknow.mineconfig.algebra.MavenCache
import com.idkidknow.mineconfig.model.MavenDep
import com.idkidknow.mineconfig.utils.IncompleteString
import fs2.io.file.Path
import io.circe.Codec
import io.circe.Decoder
import io.circe.derivation.Configuration
import io.circe.derivation.ConfiguredDecoder

final case class FunctionDescription(
    dep: MavenDep,
    repo: String,
    args: List[IncompleteString],
    jvmArgs: List[IncompleteString],
    javaVersion: Option[Int],
) {
  def cache[F[_]: {Functor, MavenCache}](
      defaultJavaVersion: Int
  ): F[FunctionDescription.Local] =
    MavenCache[F].get(dep, List(repo)).map { path =>
      FunctionDescription.Local(
        javaVersion = javaVersion.getOrElse(defaultJavaVersion),
        executableJar = path,
        args = args,
        jvmArgs = jvmArgs,
      )
    }
}

object FunctionDescription {
  final case class Local(
      javaVersion: Int,
      executableJar: Path,
      args: List[IncompleteString],
      jvmArgs: List[IncompleteString],
  )
  
  object Local {
    import com.idkidknow.mineconfig.utils.codecPath
    given Codec[Local] = Codec.derived
  }

  given Decoder[FunctionDescription] = {
    given Decoder[MavenDep] = Decoder.decodeString.emap { str =>
      val parts = str.split(":")
      parts match {
        case Array(org, name, version) =>
          Right(MavenDep(org, name, None, None, version))
        case Array(org, name, version, classifier) =>
          Right(MavenDep(org, name, Some("jar"), Some(classifier), version))
        case _ => Left("Invalid dependency string")
      }
    }
    given Configuration = Configuration.default.withTransformMemberNames {
      case "dep" => "version"
      case "jvmArgs" => "jvmargs"
      case "javaVersion" => "java_version"
      case name => name
    }
    ConfiguredDecoder.derived
  }
}
