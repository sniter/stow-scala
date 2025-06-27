// //> using platform native
// //> using nativeVersion 0.5.8
//> using scala 3.5.0
//> using dep org.typelevel::cats-core:2.13.0

import cats.syntax.all._
import java.io.File
import scala.jdk.CollectionConverters.*

// type Parameter[A]:
//   def apply(): A
//
// trait Parameter[A]:
//   def apply(value: String)

sealed trait Relation
case class Processed(file: File, ref: File) extends Relation
case class Initial(file: File) extends Relation

trait CliParser[+A]:
  def help: String
  def unapply(args: List[String]): Option[A]

sealed trait Command
object Command:
  case class Stow(
      dotFiles: Boolean = false,
      targetPath: Option[File] = None,
      sourcePath: Option[File] = None
  ) extends Command
  object AsStow extends CliParser[Stow]:
    def unapply(args: List[String]): Option[Stow] = parse(args, none)
    def help: String =
      s"""
        |-d DIR     Set stow dir to DIR (default is current dir)
        |-t DIR     Set target to DIR (default is parent of stow dir)
        |
        |--dotfiles Enables special handling for dotfiles that are
        |           Stow packages that start with "dot-" and not "."
        |
        |""".stripMargin

  private def parse(args: List[String], payload: Option[Stow]): Option[Stow] =
    payload -> args match
      case None -> ("stow" :: otherArgs) => parse(otherArgs, Stow().some)
      case Some(ctx) -> ("-t" :: targetPath :: otherArgs)
          if ctx.targetPath.isEmpty =>
        parse(otherArgs, ctx.copy(targetPath = new File(targetPath).some).some)
      case Some(ctx) -> ("-d" :: sourcePath :: otherArgs)
          if ctx.sourcePath.isEmpty =>
        parse(otherArgs, ctx.copy(sourcePath = new File(sourcePath).some).some)
      case Some(ctx) -> ("--dotfiles" :: otherArgs) =>
        parse(otherArgs, ctx.copy(dotFiles = true).some)
      case _ => none

// object StowFiles:
//   def apply(config: Stow): Unit =

// object Main:
//   def main(args: Array[String]) =
//     println("Hello from Scala Native + GraalVm ")
