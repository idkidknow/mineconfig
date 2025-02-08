package com.idkidknow.mineconfig.utils

import cats.MonadThrow
import cats.syntax.all.*

extension [A](option: Option[A]) {
  def expect[F[_]: MonadThrow](msg: String): F[A] = {
    option.toRight(new RuntimeException(msg)).pure[F].rethrow
  }
}
