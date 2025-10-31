ThisBuild / organization := "de.halcony"
ThisBuild / organizationName := "halcony"
ThisBuild / organizationHomepage := Some(url("https://koch.science"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/your-account/your-project"),
    "scm:git@github.com:your-account/your-project.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "Your identifier",
    name = "Your Name",
    email = "your@email",
    url = url("http://your.url")
  )
)

ThisBuild / description := "Some description about your project."
ThisBuild / licenses := List(
  "MIT" -> new URL("https://opensource.org/license/mit")
)

ThisBuild / homepage := Some(url("https://github.com/example/project"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true

// new setting for the Central Portal
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}