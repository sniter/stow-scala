package io.sniter

import cats.syntax.either.*
import cats.data.Ior

object Diff:
  def apply(
      left: List[Resource],
      right: List[Resource]
  ): List[Ior[Resource, Resource]] =
    // NOTE: for the future left keys and right keys should be evaluated differently (i.e. comparison dot-file and .file)
    (
      left.map(_.asLeft) ++
        right.map(_.asRight)
    ).groupBy(_.merge.name.toString)
      .values
      .collect:
        case Left(left) :: Nil => Ior.left(left)
        case Left(left) :: Right(right) :: Nil =>
          Ior.both(left, right)
        case Right(right) :: Nil => Ior.right(right)
      .toList
