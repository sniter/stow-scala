package io.sniter

import java.nio.file.Path
import java.nio.file.Files

sealed trait Resource:
  def path: Path
  def name: String = path.getFileName.toString
object Resource:
  case class File(path: Path) extends Resource
  case class Directory(path: Path) extends Resource
  case class Ref(path: Path, target: Path) extends Resource

  def apply(path: Path): Resource =
    if (Files.isSymbolicLink(path))
      Ref(path, Files.readSymbolicLink(path))
    else if (Files.isDirectory(path))
      Directory(path)
    else
      File(path)
