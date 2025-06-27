// scalaVersion := "3.3.3" // A Long Term Support version.
//
// enablePlugins(ScalaNativePlugin)
//
// // set to Debug for compilation details (Info is default)
// logLevel := Level.Info
//
// // import to add Scala Native options
// import scala.scalanative.build._
//
// // defaults set with common options shown
// nativeConfig ~= { c =>
//   c.withLTO(LTO.none) // thin
//     .withMode(Mode.debug) // releaseFast
//     .withGC(GC.immix) // commix
// }
// libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0"
// libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test

val scala3Version = "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-stow",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-explain",
      "-Yexplain-lowlevel",
      "-Wunused:all"
      // "-rewrite",
      // "-source",
      // "3.0-migration",
      // "-new-syntax"
    ),
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
  )
