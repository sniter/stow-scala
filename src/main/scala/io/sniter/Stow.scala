package io.sniter

import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.compose.*
import cats.syntax.foldable.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import java.nio.file.Path
import cats.MonadThrow
import cats.ApplicativeThrow
import cats.Monad
import cats.data.Writer
import cats.data.WriterT

trait Stow[F[_]]:
  def apply(
      source: Path,
      destination: Path,
  ): F[Unit]

object Stow:
  def apply[F[_]](using stow: Stow[F]): Stow[F] = stow

  case class Config(
      followSymLinks: Boolean,
      // dryRun: Boolean, // TODO: Implement
      // verbose: Boolean, // TODO: Implement
  )
  object Config:
    val default: Config = Config(followSymLinks = false)

  private def stow[F[_]: MonadThrow](scanner: Scan[F], src: Path, dest: Path)(using
      Diff.FileNameReader,
  ): F[Unit] =
    for
      left  <- scanner(src)
      right <- scanner(dest)
      diff = Diff(
        left.map(Resource.apply >>> Resource.Source.apply),
        right.map(Resource.apply >>> Resource.Target.apply),
      )
      _ <- diff
        .collectFirst { case Diff.Result.Error(error) => error }
        .traverse_(_.raiseError)
      conflicts = diff.collect:
        case Diff.Result.Conflict(left, right) => s"${left.path.toString} != ${right.path.toString}"
      _ <- conflicts match
        case Nil      => ().pure
        case nonEmpty => new Exception(s"Found conflicts:\n${nonEmpty.mkString("- ", "\n", "\n")}").raiseError
    // TODO: Extract nested objects only
    yield ()

  def instance[F[_]: MonadThrow](scanner: Scan[F], config: Config): Stow[F] =
    new Stow[F]:
      def apply(
          source: Path,
          destination: Path,
      ): F[Unit] =
        given Diff.FileNameReader = if (config.followSymLinks)
          Diff.FileNameReader.FollowSymLink
        else Diff.FileNameReader.ByFileName
        stow(scanner, source, destination)
