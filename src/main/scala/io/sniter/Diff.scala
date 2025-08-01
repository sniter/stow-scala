package io.sniter

import cats.syntax.either.*
import cats.syntax.eq.*
import cats.Eq
import java.nio.file.Path

object Diff:
  given Eq[Path] = Eq.fromUniversalEquals

  trait FileNameReader:
    def apply(resource: Resource.Directed): String

  object FileNameReader:
    object ByFileName extends FileNameReader:
      def apply(resource: Resource.Directed): String =
        resource.value.name

    object FollowSymLink extends FileNameReader:
      def apply(resource: Resource.Directed): String =
        resource match
          case Resource.Source(res) =>
            res.name match
              case s"dot-${name}" => s".$name"
              case other          => other
          case resource =>
            resource.value.name

  sealed trait Result
  object Result:
    case class Left(left: Resource)                      extends Result
    case class Both(left: Resource, right: Resource)     extends Result
    case class Right(right: Resource)                    extends Result
    case class Conflict(left: Resource, right: Resource) extends Result
    case class Error(error: Throwable)                   extends Result

  private def diff(
      left: List[Resource.Directed],
      right: List[Resource.Directed],
      readFileName: FileNameReader,
  ): List[Result] =
    (
      left.map(l => readFileName(l) -> l.value.asLeft) ++
        right.map(r => readFileName(r) -> r.value.asRight)
    ).groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .values
      .map:
        case Left(left) :: Nil   => Result.Left(left)
        case Right(right) :: Nil => Result.Right(right)
        case Left(left) :: Right(right @ Resource.Ref(_, target)) :: Nil if target === left.path =>
          Result.Both(left, right)
        case Left(left @ Resource.Directory(_)) :: Right(right @ Resource.Directory(_)) :: Nil =>
          Result.Both(left, right)
        case Left(left) :: Right(right) :: Nil =>
          Result.Conflict(left, right)
        case found =>
          // NOTE: This cannot happen as we cannot have two files with the same name
          val lefts = found
            .collect:
              case Left(left) => left.path.toString
            .mkString("- ", "\n", "\n")
          val rights = found
            .collect:
              case Right(right) => right.path.toString
            .mkString("- ", "\n", "\n")
          val error = s"Got unexpected state in source folder:\n$lefts\nin target folder:\n$rights\n"
          Result.Error(new RuntimeException(error))
      .toList

  def apply(
      left: List[Resource.Directed],
      right: List[Resource.Directed],
  )(using fileNameReader: FileNameReader): List[Result] =
    diff(left, right, fileNameReader)
