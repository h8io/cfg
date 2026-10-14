import Dependencies.*
import h8io.sbt.dependencies.*
import sbt.*

val ProjectName = "cfg"

organization := "io.h8"
organizationName := "H8IO"
organizationHomepage := Some(uri(s"https://github.com/h8io/$ProjectName"))
homepage := Some(uri(s"https://github.com/h8io/$ProjectName"))
scmInfo := Some(ScmInfo(uri(s"https://github.com/h8io/$ProjectName"), s"scm:git@github.com:h8io/$ProjectName.git"))
licenses := List(License.Apache2)
developers := List(
  Developer(id = "eshu", name = "Pavel", email = "tjano.xibalba@gmail.com", url = uri("https://github.com/eshu/")))

versionScheme := Some("semver-spec")

coverageSummaryStmtLowThreshold := 100
coverageSummaryStmtHighThreshold := 100
coverageSummaryBranchLowThreshold := 100
coverageSummaryBranchHighThreshold := 100

// sbt-dynver computes the version once for the whole build, from these two in ThisBuild
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"

libraryDependencies +=
  compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.4").cross(CrossVersion.full))

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

scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Scalac2Options ++ Seq("-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn", "-Ypartial-unification")
    case Some((2, 13)) => Scalac2Options ++ Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code")
    case _ => Nil
  })

javacOptions ++= Seq("--release", "11")

scalaVersion := "2.13.18"
crossScalaVersions += "2.12.21"

libraryDependencies ++= TestBundle % Test
libraryDependencies ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => Seq(ScalaCollectionCompat)
    case _ => Nil
  })

val cfg = (project in file("cfg"))
  .settings(name := ProjectName)

val schema = (project in file("schema"))
  .settings(name := "cfg-schema", libraryDependencies ++= Seq(Cats, IzumiReflect))
  .dependsOn(cfg)

val hocon = (project in file("impl/hocon"))
  .settings(name := "cfg-hocon", libraryDependencies += Config)
  .dependsOn(cfg)

val yaml = (project in file("impl/yaml"))
  .settings(name := "cfg-yaml", libraryDependencies += SnakeYaml)
  .dependsOn(cfg)

val root = (project in file("."))
  .enablePlugins(ScoverageSummaryPlugin)
  .settings(name := "cfg-all", publish / skip := true)
  .aggregate(cfg, hocon, schema, yaml)

val pages = (project in file("pages"))
  .settings(
    name := "cfg-pages",
    publish / skip := true,
    publishLocal / skip := true,
    // sbt 2 moves every target under the root's target/out; npm and release.sh expect the site's inputs here
    cleanFiles += baseDirectory.value / "target",
    ScalaUnidoc / unidoc / target := baseDirectory.value / "target" / s"scala-${scalaBinaryVersion.value}" / "unidoc",
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(root),
    // sbt-unidoc concatenates the modules' separately resolved classpaths, so snakeyaml-engine 3.0.1 (pulled by
    // typesafe-config-yaml into hocon) shadows 3.2 (yaml). Use this project's own classpath, resolved as a whole.
    ScalaUnidoc / unidoc / fullClasspath := (Compile / fullClasspath).value,
    mdocIn := (ThisBuild / baseDirectory).value / "docs",
    mdocOut := baseDirectory.value / "target" / "mdoc",
    mdocVariables :=
      Map("VERSION" -> (if (isSnapshot.value) previousStableVersion.value else None).getOrElse(version.value))
  )
  .dependsOn(cfg, hocon, schema, yaml)
  .enablePlugins(ScalaUnidocPlugin, MdocPlugin)
