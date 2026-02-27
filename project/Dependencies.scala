import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val Cats = "org.typelevel" %% "cats-core" % CatsVersion

  val Config = "com.typesafe" % "config" % "1.4.6"

  val ScalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0"

  val H8IOReflect = "io.h8" %% "reflect" % "0.0.4"

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.19",
      "org.scalamock" %% "scalamock" % "7.5.5",
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.typelevel" %% "discipline-scalatest" % "2.3.0",
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0",
      "org.scalacheck" %% "scalacheck" % "1.19.0"
    )
}
