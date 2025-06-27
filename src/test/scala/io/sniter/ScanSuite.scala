package io.sniter

import cats.Id

import java.nio.file.Files
import java.nio.file.Path
import scala.util.Random

class ScanSuite extends munit.FunSuite:
  import ScanSuite.*

  test("Scan existing folders"):
    val rootDir = makeDir
    val folder = rootDir.addFolder
    assertEquals(Scan[Id](rootDir), List(folder))

  test("Scan existing files"):
    val rootDir = makeDir
    val file = rootDir.addFile
    assertEquals(Scan[Id](rootDir), List(file))

  test("Scan symlinks without following them"):
    val rootDir = makeDir
    val file = rootDir.addFile
    val symlink = rootDir.addSymLink(file)
    assertEquals(
      Scan[Id](rootDir).sorted,
      List(file, symlink).sorted
    )

  test("Scan just first level, do not scan nested structures"):
    val rootDir = makeDir
    val folder1 = rootDir.addFolder
    val folder2 = folder1.addFolder
    val folder3 = folder2.addFolder
    val folder4 = folder3.addFolder
    val file1 = folder4.addFile
    val folder5 = folder4.addFolder
    folder4.addSymLink(folder5)
    folder4.addSymLink(file1)

    val expected = List(
      folder1
    )
    assertEquals(
      Scan[Id](rootDir).sorted,
      expected.sorted
    )

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

  extension (path: Path)
    def addFile = Files.createTempFile(path, FILE_PREFIX, FILE_SUFFIX)
    def addFolder = Files.createTempDirectory(path, FILE_PREFIX)
    def addSymLink(source: Path) = Files.createSymbolicLink(
      path.resolve(FILE_PREFIX + alphaNum(8) + SYMLINK_SYFFIX),
      source
    )
