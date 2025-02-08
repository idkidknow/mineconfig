package com.idkidknow.mineconfig.utils

import cats.syntax.all.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder

/** An ASCII string that contains something like "{var}" */
final case class IncompleteString(raw: String) {

  /** returns None if there's missing variable */
  def fill(vars: Map[String, String]): Option[String] = {
    val ret = vars.foldLeft(raw) { case (s, (k, v)) =>
      s.replace(show"{$k}", v)
    }
    if (ret.contains("{")) None else Some(ret)
  }
}

object IncompleteString {
  given Codec[IncompleteString] = Codec.from(
    Decoder.decodeString.map(IncompleteString(_)),
    Encoder.encodeString.contramap(_.raw),
  )
}
