import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val Cats = "org.typelevel" %% "cats-core" % CatsVersion

  val Config = "io.h8" % "typesafe-config-yaml" % "1.2.1"

  val ScalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0"

  val IzumiReflect = "dev.zio" %% "izumi-reflect" % "3.0.10"

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.20",
      "org.scalamock" %% "scalamock-scalatest" % "7.6.0",
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.typelevel" %% "discipline-scalatest" % "2.3.0",
      "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0",
      "org.scalacheck" %% "scalacheck" % "1.20.0"
    )
}
