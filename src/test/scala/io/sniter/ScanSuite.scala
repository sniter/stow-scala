package io.sniter

import java.nio.file.Files
import cats.Eval
import java.nio.file.Path
import scala.util.Random
import scala.util.Try
import java.nio.file.FileSystem

class ScanSuite extends munit.FunSuite:
  import ScanSuite.{*, given}
  test("Scan existing folders"):
    val rootDir = makeDir
    val folder = rootDir.addFolder
    assertEquals(Scan(rootDir), List(folder))

  test("Scan existing files"):
    val rootDir = makeDir
    val file = rootDir.addFile
    assertEquals(Scan(rootDir), List(file))

  test("Scan symlinks without following them"):
    val rootDir = makeDir
    val file = rootDir.addFile
    val symlink = rootDir.addSymLink(file)
    assertEquals(Scan(rootDir).sorted, List(file, symlink).sorted)

  test("Scan symlinks with following"):
    val rootDir = makeDir
    val file = rootDir.addFile
    val symlink = rootDir.addSymLink(file)
    assertEquals(Scan(rootDir, followSymLinks = true), List(file, file))

  test("Scan nested structures"):
    val rootDir = makeDir
    val folder1 = rootDir.addFolder
    val folder2 = folder1.addFolder
    val folder3 = folder2.addFolder
    val folder4 = folder3.addFolder
    val file1 = folder4.addFile
    val folder5 = folder4.addFolder
    val symlink1 = folder4.addSymLink(folder5)
    val symlink2 = folder4.addSymLink(file1)

    val expected = List(
      folder1,
      folder2,
      folder3,
      folder4,
      folder5,
      file1,
      symlink1,
      symlink2
    )
    assertEquals(Scan(rootDir).sorted, expected.sorted)

object ScanSuite:

  private val FILE_PREFIX = "test-"
  private val FILE_SUFFIX = ".file"
  private val SYMLINK_SYFFIX = ".symlink"

  private def makeDir = Files.createTempDirectory("scan-suite-")
  private def alphaNum(length: Int): String =
    val symbols = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ1234567890"
    Range(0, length)
      .map(_ => Random.between(0, symbols.size))
      .map(symbols.charAt)
      .mkString

  given Ordering[Path] = Ordering.by(_.toString)

  extension (path: Path)
    def addFile = Files.createTempFile(path, FILE_PREFIX, FILE_SUFFIX)
    def addFolder = Files.createTempDirectory(path, FILE_PREFIX)
    def addSymLink(source: Path) = Files.createSymbolicLink(
      path.resolve(FILE_PREFIX + alphaNum(8) + SYMLINK_SYFFIX),
      source
    )
