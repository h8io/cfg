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

// Every branch on the Scala version lists the versions it knows and ends here, so that a new one fails the build until
// each of them has been looked at
def unsupportedScala(version: String): Nothing = throw new MessageOnlyException(s"Unsupported Scala version $version")

scalaVersion := "2.13.18"
crossScalaVersions ++= Seq("2.12.21", "3.9.0")
javacOptions ++= Seq("--release", "11")

libraryDependencies ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) | Some((2, 12)) =>
      Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.4").cross(CrossVersion.full)))
    case Some((3, 9)) => Nil
    case _ => unsupportedScala(scalaVersion.value)
  })

scalacOptions ++= {
  val scala2 = Seq("-Xsource:3", "--deprecation", "--feature", "--unchecked", "-Xlint:_", "-Xfatal-warnings",
    "-opt:l:inline", "-opt-warnings")
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) =>
      scala2 ++ Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code", "-Wvalue-discard",
        "-Wnonunit-statement")
    case Some((2, 12)) =>
      scala2 ++ Seq("-language:higherKinds", "-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn",
        "-Ywarn-value-discard", "-Ypartial-unification")
    case Some((3, 9)) =>
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Wall",
        "-Werror",
        "-explain",
        "-opt",
        "-Wopt:at-inline-failed-summary",
        // Scala 2 needs the trailing `_` to eta-expand a method without an expected function type, Scala 3 does not
        "-Wconf:msg=eta-expansion is unnecessary:s"
      )
    case _ => unsupportedScala(scalaVersion.value)
  }
}

// For code whose statements are values on purpose: a ScalaMock expectation or a ScalaTest assertion in tests, an
// expression shown with its result in the docs. Those are spared the warnings about discarded values. On 2.13 the
// category of -Wnonunit-statement is shared with the warning about a pure expression in statement position, which
// they lose as well.
def lenientScalacOptions(scalaVersion: String): Seq[String] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => Seq("-Wconf:cat=w-flag-value-discard:s,cat=other-pure-statement:s")
    case Some((2, 12)) => Seq("-Wconf:cat=w-flag-value-discard:s")
    case Some((3, 9)) => Seq("-Wconf:name=ValueDiscarding:s,name=UnusedNonUnitValue:s")
    case _ => unsupportedScala(scalaVersion)
  }

Test / scalacOptions ++= lenientScalacOptions(scalaVersion.value)
// Tests are registered while the suite is being constructed, so Scala 3's initialization checker (part of -Wall) cannot
// prove that a test body or a law's Eq, which close over the suite, only run once it is fully initialized
Test / scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) | Some((2, 12)) => Nil
    case Some((3, 9)) => Seq("-Wconf:msg=transitively initialized:s")
    case _ => unsupportedScala(scalaVersion.value)
  })

libraryDependencies ++= TestBundle % Test
libraryDependencies ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => Seq(ScalaCollectionCompat)
    case Some((2, 13)) | Some((3, 9)) => Nil
    case _ => unsupportedScala(scalaVersion.value)
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
    scalacOptions ++= lenientScalacOptions(scalaVersion.value),
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
