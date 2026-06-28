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

ThisBuild / javacOptions ++= Seq("--release", "11")

ThisBuild / scalaVersion := "2.13.18"
ThisBuild / crossScalaVersions += "2.12.21"

ThisBuild / libraryDependencies ++= TestBundle % Test
ThisBuild / libraryDependencies ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => Seq(ScalaCollectionCompat)
    case _ => Nil
  })

val cfg = (project in file("cfg"))
  .settings(name := ProjectName)

val schema = (project in file("schema"))
  .settings(name := "cfg-schema", libraryDependencies ++= Seq(Cats, H8IOReflect))
  .dependsOn(cfg)

val hocon = (project in file("impl/hocon"))
  .settings(name := "cfg-hocon", libraryDependencies += Config)
  .dependsOn(cfg)

val root = (project in file("."))
  .enablePlugins(ScoverageSummaryPlugin)
  .settings(name := "cfg-all", publish / skip := true)
  .aggregate(cfg, hocon, schema)
