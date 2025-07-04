package io.sniter

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.data.Ior
import cats.Show
import cats.Eq
import java.nio.file.Path

object Diff:
  given Eq[Path] = Eq.fromUniversalEquals

  private val getFileName: Resource.Directed => String =
    _.value.path.getFileName.toString

  private def diff[R <: Resource](
      left: List[Resource.Directed],
      right: List[Resource.Directed],
      keyEncode: Resource.Directed => String,
  ): List[Ior[Resource, Resource]] =
    (
      left.map(l => keyEncode(l) -> l.value.asLeft) ++
        right.map(r => keyEncode(r) -> r.value.asRight)
    ).groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .values
      .flatMap:
        case Left(left) :: Nil                 => Ior.left(left).pure[List]
        case Right(right) :: Nil               => Ior.right(right).pure[List]
        case Left(left) :: Right(right @ Resource.Ref(_, target)) :: Nil
             if target === left.path =>
          Ior.both(left, right).pure[List]
        case Left(left @ Resource.Directory(_)) :: Right(
               right @ Resource.Directory(_),
             ) :: Nil =>
          Ior.both(left, right).pure[List]
        case Left(left) :: Right(right) :: Nil =>
          List(
            Ior.Left(left),
            Ior.Right(right),
          )
        case found                             =>
          // NOTE: Fallback case if more than two files per single name exists
          val lefts  = found.collect:
            case Left(left) => Resource.Source(left)
          val rights = found.collect:
            case Right(right) => Resource.Target(right)
          diff(lefts, rights, getFileName)
      .toList

  def apply[R <: Resource](
      left: List[Resource.Directed],
      right: List[Resource.Directed],
  )(using Show[Resource.Directed]): List[Ior[Resource, Resource]] =
    diff(left, right, Show[Resource.Directed].show)
