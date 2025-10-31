name := "conf-parser"
ThisBuild / scalaVersion := "3.6.4"
organization := "de.halcony"
licenses += "MIT" -> url("https://opensource.org/license/mit")

enablePlugins(JavaAppPackaging)

val cpgVersion = "1.7.41"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest"              %% "scalatest"                  % "3.2.19"   % Test,
  "de.halcony"                 %% "scala-argparse"             % "2.0.5",
  "io.spray"                   %% "spray-json"                 % "1.3.6",
  "org.wvlet.airframe"         %% "airframe-log"               % "2025.1.21",
  "com.lihaoyi"                %% "fastparse"                  % "3.1.1"
)

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.mavenCentral,
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/public",
  "Apache public" at "https://repository.apache.org/content/groups/public/"
)

ThisBuild / scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-explain-cyclic",
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork := false
run / fork := true
cancelable in Global := true
Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")



