import sbt.*

object Dependencies {
  val Config = "io.h8" % "typesafe-config-yaml" % "1.2.2"

  val SnakeYaml = "org.snakeyaml" % "snakeyaml-engine" % "3.2"

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.20",
      "org.scalamock" %% "scalamock-scalatest" % "7.6.3",
      "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0",
      "org.scalacheck" %% "scalacheck" % "1.20.0"
    )
}
