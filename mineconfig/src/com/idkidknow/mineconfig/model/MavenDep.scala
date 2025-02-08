package com.idkidknow.mineconfig.model

final case class MavenDep(
    groupId: String,
    artifactId: String,
    `type`: Option[String],
    classifier: Option[String],
    version: String,
)
