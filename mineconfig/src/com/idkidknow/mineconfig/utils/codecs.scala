package com.idkidknow.mineconfig.utils

import cats.syntax.all.*
import io.circe.Decoder
import org.http4s.Uri

given decoderUri: Decoder[Uri] = Decoder.decodeString.emap { str =>
  Uri.fromString(str).leftMap(_.getMessage)
}
