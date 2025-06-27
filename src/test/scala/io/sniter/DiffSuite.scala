package io.sniter

import java.nio.file.Path
import cats.data.Ior

class DiffSuite extends munit.FunSuite:
  import DiffSuite.*
  import syntax.given

  test("Simple diff"):
    val src = List(
      "/t1/a/b/a1",
      "/t1/a/b/a2",
      "/t1/a/b/a3",
      "/t1/a/b/a4",
      "/t1/a/b/a6"
    ).map(file)
    val dest = List(
      "/t2/a/b/a2",
      "/t2/a/b/a4",
      "/t2/a/b/a5",
      "/t2/a/b/a6",
      "/t2/a/b/a7"
    ).map(file)
    val actual = Diff(src, dest)
    val expected = List(
      Ior.Left(file("/t1/a/b/a1")),
      Ior.Left(file("/t1/a/b/a3")),
      Ior.Both(file("/t1/a/b/a2"), file("/t2/a/b/a2")),
      Ior.Both(file("/t1/a/b/a4"), file("/t2/a/b/a4")),
      Ior.Both(file("/t1/a/b/a6"), file("/t2/a/b/a6")),
      Ior.Right(file("/t2/a/b/a5")),
      Ior.Right(file("/t2/a/b/a7"))
    )
    assertEquals(actual.sorted, expected.sorted)

object DiffSuite:
  def file(f: String): Resource =
    Resource.File(Path.of(f))
