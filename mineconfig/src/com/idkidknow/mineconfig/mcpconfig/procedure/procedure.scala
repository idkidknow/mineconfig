package com.idkidknow.mineconfig.mcpconfig.procedure

import cats.effect.Concurrent
import cats.effect.implicits.*
import cats.syntax.all.*
import com.idkidknow.mineconfig.algebra.MavenCache
import com.idkidknow.mineconfig.algebra.StringRW
import com.idkidknow.mineconfig.algebra.ZipFile
import com.idkidknow.mineconfig.mcpconfig.model.FunctionDescription
import com.idkidknow.mineconfig.utils.*
import fs2.io.file.Path
import io.circe.Decoder
import io.circe.Json
import io.circe.optics.JsonPath.root
import io.circe.syntax.*

private def readConfigJson[F[_]: Concurrent](zipFile: ZipFile[F]): F[Json] = {
  val str: F[String] = zipFile.readEntry("config.json")
    .through(fs2.text.utf8.decode)
    .compile.string
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
