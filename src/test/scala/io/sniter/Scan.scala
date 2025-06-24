package io.sniter

import java.nio.file.Path

class ScanSuite extends munit.FunSuite:
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
    val actual = Scan.diff(Path.of("/t1"), src, Path.of("/t2"), dest)
    val expected = List(
      Scan.Diff.Left(Path.of("/t1/a/b/a1")),
      Scan.Diff.Equal(Path.of("/t1/a/b/a2"), Path.of("/t2/a/b/a2")),
      Scan.Diff.Left(Path.of("/t1/a/b/a3")),
      Scan.Diff.Equal(Path.of("/t1/a/b/a4"), Path.of("/t2/a/b/a4")),
      Scan.Diff.Right(Path.of("/t2/a/b/a5")),
      Scan.Diff.Equal(Path.of("/t1/a/b/a6"), Path.of("/t2/a/b/a6")),
      Scan.Diff.Right(Path.of("/t2/a/b/a7"))
    )
    assertEquals(actual, expected)
