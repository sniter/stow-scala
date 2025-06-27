package io.sniter

import cats.data.Ior

object syntax:
  given Ordering[Ior[Resource, Resource]] = Ordering.by:
    case Ior.Left(left)    => left.name
    case Ior.Right(right)  => right.name
    case Ior.Both(left, _) => left.name
