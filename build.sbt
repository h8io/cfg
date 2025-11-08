import Dependencies.*
import h8io.sbt.dependencies.*
import sbt.*

val ProjectName = "cfg"

ThisBuild / organization := "io.h8"
ThisBuild / organizationName := "H8IO"
ThisBuild / organizationHomepage := Some(url(s"https://github.com/h8io/$ProjectName"))
ThisBuild / homepage := Some(url(s"https://github.com/h8io/$ProjectName"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url(s"https://github.com/h8io/$ProjectName"), s"scm:git@github.com:h8io/$ProjectName.git"))

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

ThisBuild / developers := List(
  Developer(
    id = "eshu",
    name = "Pavel",
    email = "tjano.xibalba@gmail.com",
    url = url("https://github.com/eshu/")))

ThisBuild / versionScheme := Some("semver-spec")

ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"

ThisBuild / libraryDependencies +=
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.full)

val Scalac2Options = Seq(
  "-Xsource:3",
  "-language:higherKinds",
  "--deprecation",
  "--feature",
  "--unchecked",
  "-Xlint:_",
  "-Xfatal-warnings",
  "-opt:l:inline",
  "-opt-warnings")

ThisBuild / scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Scalac2Options ++ Seq("-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn", "-Ypartial-unification")
    case Some((2, 13)) => Scalac2Options ++ Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code")
    case _ => Nil
  })

ThisBuild / javacOptions ++= Seq("-target", "8")

ThisBuild / scalaVersion := "2.13.17"
ThisBuild / crossScalaVersions += "2.12.20"

ThisBuild / libraryDependencies ++= TestBundle % Test

val raw = (project in file("raw")).settings(name := "cfg-raw")

val hocon = (project in file("raw/hocon"))
  .settings(name := "cfg-raw-hocon", libraryDependencies ++= Seq(Config, ScalaCollectionCompat))
  .dependsOn(raw)

val root = (project in file(".")).enablePlugins(ScoverageSummaryPlugin).settings(
  name := ProjectName,
  libraryDependencies ++= Seq(Cats, Config, ScalaCollectionCompat)
).dependsOn(raw).aggregate(raw, hocon)
