package io.sniter

import cats.Id
import cats.Eval
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.util.stream.Collectors

import scala.jdk.CollectionConverters.*

trait Scan[F[_]]:
  def apply(target: Path): F[List[Path]]

object Scan:
  def apply[F[_]](using scanF: Scan[F]): Scan[F] = scanF

  given Scan[Id] = new:
    def apply(target: Path): List[Path] =
      scan(target)

  given Scan[Eval] with
    def apply(target: Path): Eval[List[Path]] =
      Eval.always(scan(target))

  private def follow(followSymLinks: Boolean): Path => Path =
    if (!followSymLinks) identity
    else
      _ match
        case file if Files.isSymbolicLink(file) =>
          Files.readSymbolicLink(file)
        case file => file

  private def scan(target: Path, followSymLinks: Boolean = false): List[Path] =
    Files
      .walk(target, 1)
      .collect(Collectors.toList)
      .asScala
      .toList
      .map(follow(followSymLinks)) match
      case _ :: tail => tail // getting rid of root folder
      case other     => other
