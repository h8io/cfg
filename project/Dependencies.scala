import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val Cats = "org.typelevel" %% "cats-core" % CatsVersion

  val Config = "com.typesafe" % "config" % "1.4.5"

  val ScalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0"

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.19",
      "org.scalamock" %% "scalamock" % "7.5.0"
    )
}
