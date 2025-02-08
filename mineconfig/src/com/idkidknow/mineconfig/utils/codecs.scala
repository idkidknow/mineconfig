package com.idkidknow.mineconfig.utils

import cats.syntax.all.*
import fs2.io.file.Path
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import org.http4s.Uri

given decoderUri: Decoder[Uri] = Decoder.decodeString.emap { str =>
  Uri.fromString(str).leftMap(_.getMessage)
}

given codecPath: Codec[Path] = Codec.from(
  Decoder.decodeString.map(str => Path(str)),
  Encoder.encodeString.contramap(path => path.toString),
)
