package com.idkidknow.mineconfig.algebra

import cats.effect.kernel.Async
import cats.syntax.all.*
import com.idkidknow.mineconfig.model.MavenDep
import com.idkidknow.mineconfig.utils.expect
import coursier.Attributes
import coursier.Classifier
import coursier.Dependency
import coursier.Fetch
import coursier.MavenRepository
import coursier.Module
import coursier.ModuleName
import coursier.Organization
import coursier.Type
import fs2.io.file.Path

trait MavenCache[F[_]] {
  def get(dep: MavenDep, repositories: List[String]): F[Path]
}

object MavenCache {
  def apply[F[_]: MavenCache as inst]: MavenCache[F] = inst

  def coursier[F[_]: Async as F]: MavenCache[F] = (dep, repositories) => {
    val coursierDep = {
      val base = Dependency(
        Module(Organization(dep.groupId), ModuleName(dep.artifactId)),
        dep.version,
      )
      (dep.`type`, dep.classifier) match {
        case (Some(ty), Some(classifier)) =>
          base.withAttributes(Attributes(Type(ty), Classifier(classifier)))
        case (Some(ty), None) => base.withAttributes(Attributes(Type(ty)))
        case (None, Some(classifier)) =>
          base.withAttributes(Attributes(Type.jar, Classifier(classifier)))
        case (None, None) => base
      }
    }
    val future = F.delay {
      Fetch()
        .addRepositories(repositories.map(MavenRepository(_))*)
        .addDependencies(coursierDep)
        .future()
    }
    val fOptFile: F[Option[java.io.File]] =
      F.fromFuture(future).map(_.headOption)
    for {
      optFile <- fOptFile
      file <- optFile.expect(show"failed to get ${dep.toString}")
    } yield Path.fromNioPath(file.toPath)
  }
}
