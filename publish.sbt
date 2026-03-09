ThisBuild / organization := "de.halcony"
ThisBuild / organizationName := "halcony"
ThisBuild / homepage := Some(url("https://koch.science"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/simkoc/scala-conf-parser"),
    "scm:git@github.com:simkoc/scala-conf-parser.git"
  )
)

// this is required for sonatype sync requirements
ThisBuild / developers := List(
  Developer(
    id    = "simkoc",
    name  = "Simon Koch",
    email = "ossrh@halcony.de",
    url   = url("https://koch.science")
  )
)

// this is required for sonatype sync requirements
ThisBuild / description := "a set of parsers for a variety of config files"
// this is required for sonatype sync requirements
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/license/mit"))
// this is required for sonatype sync requirements
ThisBuild / homepage := Some(url("https://github.com/simkoc/scala-conf-parser"))
ThisBuild / versionScheme := Some("semver-spec")
// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
// new setting for the Central Portal
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}