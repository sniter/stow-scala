package io.sniter

import java.nio.file.Path

class DiffSuite extends munit.FunSuite:
  test("Simple diff"):
    val src = List(
      "/t1/a/b/a1",
      "/t1/a/b/a2",
      "/t1/a/b/a3",
      "/t1/a/b/a4",
      "/t1/a/b/a6"
    ).map(Path.of(_))
    val dest = List(
      "/t2/a/b/a2",
      "/t2/a/b/a4",
      "/t2/a/b/a5",
      "/t2/a/b/a6",
      "/t2/a/b/a7"
    ).map(Path.of(_))
    val actual = Diff(Path.of("/t1"), src, Path.of("/t2"), dest)
    val expected = List(
      Diff.Removed(Path.of("/t1/a/b/a1")),
      Diff.NotModified(Path.of("/t1/a/b/a2"), Path.of("/t2/a/b/a2")),
      Diff.Removed(Path.of("/t1/a/b/a3")),
      Diff.NotModified(Path.of("/t1/a/b/a4"), Path.of("/t2/a/b/a4")),
      Diff.Added(Path.of("/t2/a/b/a5")),
      Diff.NotModified(Path.of("/t1/a/b/a6"), Path.of("/t2/a/b/a6")),
      Diff.Added(Path.of("/t2/a/b/a7"))
    )
    assertEquals(actual, expected)
