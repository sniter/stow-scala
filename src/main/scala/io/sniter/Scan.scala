package io.sniter

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.syntax.partialOrder.*
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*
import cats.Defer
import cats.Applicative
import cats.Eq
import cats.kernel.PartialOrder

object Scan:
  sealed trait Diff
  object Diff:
    case class Left(left: Path) extends Diff
    case class Right(right: Path) extends Diff
    case class Equal(left: Path, right: Path) extends Diff

  def apply(target: Path): List[Path] =
    Files
      .walk(target, FileVisitOption.FOLLOW_LINKS)
      .collect(Collectors.toList)
      .asScala
      .toList

  given Eq[Path] = Eq.fromUniversalEquals
  given PartialOrder[Path] = PartialOrder.by(_.toString)

  extension (child: Path)
    def diffParent(parent: String): Path =
      if (child.startsWith(parent))
        Path.of(child.toString.replaceFirst(parent + "/", ""))
      else
        child

  private def scanForward[A: Eq](
      left: A,
      leftTail: List[A],
      right: A,
      rightTail: List[A],
      lBuffer: List[A] = Nil,
      rBuffer: List[A] = Nil
  ): Option[Either[(List[A], List[A]), (List[A], List[A])]] =
    leftTail -> rightTail match
      case Nil -> Nil => none
      case Nil -> (rHead :: rTail) if rHead === left =>
        (rBuffer.reverse, rTail).asRight.some
      case Nil -> (rHead :: rTail) =>
        scanForward(left, Nil, right, rTail, lBuffer, rHead :: rBuffer)
      case (lHead :: lTail) -> Nil if lHead === right =>
        (lBuffer.reverse, lTail).asLeft.some
      case (lHead :: lTail) -> Nil =>
        scanForward(left, lTail, right, Nil, lHead :: lBuffer, rBuffer)
      case (lHead :: lTail) -> (rHead :: rTail) if lHead === right =>
        (lBuffer.reverse, lTail).asLeft.some
      case (lHead :: lTail) -> (rHead :: rTail) if rHead === left =>
        (rBuffer.reverse, rTail).asRight.some
      case (lHead :: lTail) -> (rHead :: rTail) =>
        scanForward(
          left,
          lTail,
          right,
          rTail,
          lHead :: lBuffer,
          rHead :: rBuffer
        )

  private def makeDiff(
      src: List[Path],
      dest: List[Path],
      result: Vector[Diff]
  ): List[Diff] =
    src -> dest match
      case Nil -> Nil => result.toList
      case Nil -> _ =>
        makeDiff(Nil, Nil, dest.map(Diff.Right(_)).toVector ++ result)
      case _ -> Nil =>
        makeDiff(Nil, Nil, src.map(Diff.Left(_)).toVector ++ result)
      case (srcHead :: srcTail) -> (destHead :: destTail)
          if srcHead === destHead =>
        makeDiff(
          srcTail,
          destTail,
          Diff.Equal(srcHead, destHead) +: result
        )
      case (srcHead :: srcTail) -> (destHead :: destTail) =>
        scanForward(srcHead, srcTail, destHead, destTail) match
          case None =>
            makeDiff(
              srcTail,
              destTail,
              (if (srcHead > destHead)
                 Vector(Diff.Left(srcHead), Diff.Right(destHead))
               else Vector(Diff.Right(destHead), Diff.Left(srcHead))) ++ result
            )
          // destHead found during scan in sources
          case Some(
                Left(lBefore -> lAfter)
              ) =>
            makeDiff(
              lAfter,
              destTail,
              Diff.Equal(destHead, destHead) +:
                ((lBefore.toVector :+ srcHead).map(Diff.Left(_)) ++ result)
            )
          // srcHead found during scan in sources
          case Some(Right(rBefore -> rAfter)) =>
            makeDiff(
              srcTail,
              rAfter,
              Diff.Equal(srcHead, srcHead) +:
                ((rBefore.toVector :+ destHead).map(Diff.Right(_)) ++ result)
            )

  def diff(
      srcRoot: Path,
      src: List[Path],
      destRoot: Path,
      dest: List[Path]
  ): List[Diff] =
    makeDiff(
      src.map(_.diffParent(srcRoot.toString)),
      dest.map(_.diffParent(destRoot.toString)),
      Vector.empty
    ).reverse.map:
      case Diff.Left(left) if !left.isAbsolute =>
        Diff.Left(srcRoot.resolve(left))
      case Diff.Right(right) if !right.isAbsolute =>
        Diff.Right(destRoot.resolve(right))
      case Diff.Equal(left, right) if !left.isAbsolute =>
        Diff.Equal(srcRoot.resolve(left), destRoot.resolve(right))
      case other => other
