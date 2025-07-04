package io.sniter

import cats.syntax.compose.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.nested.*
import java.nio.file.Path
import cats.Monad
import cats.Show

trait Stow[F[_]]:
  def apply(
      source: Path,
      destination: Path,
      dryRun: Boolean,
      verbose: Boolean
  ): F[Unit]

object Stow:
  def apply[F[_]](using stow: Stow[F]): Stow[F] = stow

  def instance[F[_]: Monad](scanner: Scan[F])(using
      Show[Resource.Directed]
  ): Stow[F] =
    new Stow[F]:
      def apply(
          source: Path,
          destination: Path,
          dryRun: Boolean,
          verbose: Boolean
      ): F[Unit] =
        for
          left <- scanner(source).nested
            .map(Resource.apply >>> Resource.Source.apply)
            .value
          right <- scanner(destination).nested
            .map(Resource.apply >>> Resource.Target.apply)
            .value
          diff = Diff(left, right)
        yield ()
