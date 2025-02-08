package com.idkidknow.mineconfig.model.vanilla

import cats.syntax.all.*
import io.circe.Decoder

import scala.util.matching.Regex

/** An entry of `rules` in [[https://minecraft.wiki/w/Client.json]] */
final case class Rule(
    allow: Boolean,
    features: Map[String, Boolean],
    osName: Option[String],
    osVersionRegex: Regex,
    arch: Option[String],
) {
  def check(
      features: Map[String, Boolean],
      osInfo: OSInfo
  ): Boolean = {
    val featuresMatch = this.features.forall { case (k, v) =>
      features.getOrElse(k, false) == v
    }
    val osNameMatch = this.osName.forall(_ == osInfo.name)
    val osVersionMatch =
      this.osVersionRegex.findFirstMatchIn(osInfo.version).isDefined
    val archMatch = this.arch.forall(_ == osInfo.arch)
    val allMatch = featuresMatch && osNameMatch && osVersionMatch && archMatch
    if (allow) allMatch else !allMatch
  }
}

object Rule {
  private final case class RuleInternal(
      action: String,
      features: Option[Map[String, Boolean]],
      os: Option[OsInternal],
  )
  private final case class OsInternal(
      name: Option[String],
      version: Option[String],
      arch: Option[String],
  )

  given Decoder[Rule] = {
    given Decoder[OsInternal] = Decoder.derived
    Decoder.derived[RuleInternal].emap { internal =>
      for {
        allow <- internal.action match {
          case "allow" => Right(true)
          case "disallow" => Right(false)
          case _ => Left("Unknown action")
        }
        features = internal.features.getOrElse(Map.empty)
        osName = internal.os.flatMap(_.name)
        osVersionRegex = internal.os.flatMap(_.version)
          .map(_.r).getOrElse(".*".r)
        arch = internal.os.flatMap(_.arch)
      } yield Rule(allow, features, osName, osVersionRegex, arch)
    }
  }
}
