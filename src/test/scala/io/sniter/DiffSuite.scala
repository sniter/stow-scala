package io.sniter

import java.nio.file.Path
import cats.syntax.applicative.*

class DiffSuite extends munit.FunSuite:
  import DiffSuite.{*, given}
  import syntax.given

  test("Simple diff"):
    val src = List(
      "/t1/a/b/a1",
      "/t1/a/b/a2",
      "/t1/a/b/a3",
      "/t1/a/b/a4",
      "/t1/a/b/a6",
    ).map(file).map(Resource.Source.apply)
    val dest = List(
      ref("/t2/a/b/a2", "/t1/a/b/a2"),
      ref("/t2/a/b/a4", "/t1/a/b/a4"),
      file("/t2/a/b/a5"),
      ref("/t2/a/b/a6", "/t1/a/b/a6"),
      file("/t2/a/b/a7"),
    ).map(Resource.Target.apply)

    val actual = Diff(src, dest)

    val expected = List(
      Diff.Result.Left(file("/t1/a/b/a1")),
      Diff.Result.Left(file("/t1/a/b/a3")),
      Diff.Result.Both(file("/t1/a/b/a2"), ref("/t2/a/b/a2", "/t1/a/b/a2")),
      Diff.Result.Both(file("/t1/a/b/a4"), ref("/t2/a/b/a4", "/t1/a/b/a4")),
      Diff.Result.Both(file("/t1/a/b/a6"), ref("/t2/a/b/a6", "/t1/a/b/a6")),
      Diff.Result.Right(file("/t2/a/b/a5")),
      Diff.Result.Right(file("/t2/a/b/a7")),
    )
    assertEquals(actual.sorted, expected.sorted)

  test(
    "Files treated linked connected if target with the same name is pointing back",
  ):
    val r1    = "/to/a/b/ref"
    val r2    = "/to/a/b/another-ref"
    val sf1   = "/t1/a/b/a1"
    val sf2   = "/t1/a/b/.a2"
    val sf3   = "/t1/a/b/dot-a3"
    val tf1   = "/t2/a/b/a1"
    val tf2   = "/t2/a/b/.a2"
    val tf3   = "/t2/a/b/.a3"
    val files = List(sf1 -> tf1, sf2 -> tf2, sf3 -> tf3)

    val input = (
      files.flatMap:
        case s -> t =>
          List(
            file(s)    -> ref(t, s) -> true,
            dir(s)     -> ref(t, s) -> true,
            ref(s, r1) -> ref(t, s) -> true,
            dir(s)     -> dir(t)    -> true,
          )
    ) ++ (
      files.flatMap:
        case s -> t =>
          // Incorrect cases
          List(
            file(s)    -> file(t)    -> false,
            file(s)    -> dir(t)     -> false,
            file(s)    -> ref(t, r2) -> false,
            dir(s)     -> file(t)    -> false,
            dir(s)     -> ref(t, r2) -> false,
            ref(s, r1) -> file(t)    -> false,
            ref(s, r1) -> dir(t)     -> false,
            ref(s, r1) -> ref(t, r2) -> false,
            ref(s, r1) -> ref(t, r1) -> false,
          )
    )

    input.foreach { case left -> right -> areLinked =>
      val actual = Diff(Resource.Source(left).pure, Resource.Target(right).pure)
      if (areLinked)
        val expected = Diff.Result.Both(left, right).pure[List]
        assertEquals(actual, expected)
      else
        val expected = Diff.Result.Conflict(left, right).pure[List]
        assertEquals(actual, expected)
    }

object DiffSuite:
  def ref(f: String, t: String) = Resource.Ref(Path.of(f), Path.of(t))
  def file(f: String): Resource = Resource.File(Path.of(f))
  def dir(f: String): Resource  = Resource.Directory(Path.of(f))
  given Diff.FileNameReader     = Diff.FileNameReader.FollowSymLink
