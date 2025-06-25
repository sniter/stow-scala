package io.sniter

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.util.stream.Collectors

import scala.jdk.CollectionConverters.*

object Scan:
  def follow(followSymLinks: Boolean): Path => Path =
    if (!followSymLinks) identity
    else
      _ match
        case file if Files.isSymbolicLink(file) =>
          Files.readSymbolicLink(file)
        case file => file
  def apply(target: Path, followSymLinks: Boolean = false): List[Path] =
    Files
      .walk(target)
      .collect(Collectors.toList)
      .asScala
      .toList
      .map(follow(followSymLinks)) match
      case _ :: tail => tail // getting rid of root folder
      case other     => other
