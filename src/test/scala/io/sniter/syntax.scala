package io.sniter

object syntax:
  given Ordering[Diff.Result] = Ordering.by:
    case Diff.Result.Left(left)            => left.name
    case Diff.Result.Right(right)          => right.name
    case Diff.Result.Both(left, _)         => left.name
    case Diff.Result.Conflict(left, right) => left.name
    case Diff.Result.Error(error)          => s"\n${error.getMessage}"
