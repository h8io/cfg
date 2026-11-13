package h8io.cfg.impl.psi

import h8io.cfg.{INode, Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL

class RealisticTest extends AnyFlatSpec with Matchers {
  private def loader: PsiLoader =
    Psi.builder.withSystemProperties(Map.empty).build.fold(e => fail(e.toString), identity)

  private def resource(name: String): URL = Option(getClass.getResource("/" + name)).getOrElse(fail(name))

  private def load(names: String*): Node.IMap[Id.Root] =
    loader(names.map(resource)*).fold(errors => fail(errors.toString), identity)

  /** The value at a dotted path of bare keys, as text: a scalar's value, `null`, or what else it is. */
  private def at(root: Node.IMap[Id.Root], path: String): String =
    text(path.split('.').foldLeft[INode[Id]](root) {
      case (map: Node.IMap[?], key) => map(key)
      case (other, _) => other
    })

  private def text(node: INode[Id]): String =
    node match {
      case Node.IScalar(_, _, value, _) => value
      case Node.INull(_, _, _) => "null"
      case _: Node.INone[?] => "none"
      case other => other.getClass.getSimpleName
    }

  private def elements(root: Node.IMap[Id.Root], key: String): List[Node.ISome[Id]] =
    root(key) match {
      case seq: Node.ISeq[?] => seq.iterator.toList
      case other => fail(other.toString)
    }

  "impl/hocon's resources, ported" should "load a base with its reference included" in {
    val root = load("ported/base.psi")
    List("base-config", "reference", "source", "src", "property", "nullable", "override-config").map(
      at(root, _)) shouldBe
      List("true", "true", "reference", "reference", "12", "null", "none")
  }

  they should "override the base, substitutions resolved after the merge" in {
    val root = load("ported/base.psi", "ported/override.psi")
    List("override-config", "source", "src", "nullable").map(at(root, _)) shouldBe
      List("true", "overridden", "overridden", "non-null")
  }

  they should "keep an optional include optional" in {
    at(load("ported/including.psi"), "including") shouldBe "true"
  }

  they should "wrap maps, sequences, scalars and nulls" in {
    val root = load("ported/wrap-test.psi")
    List("map.a", "map.b", "map.c", "map.d", "scalar", "null").map(at(root, _)) shouldBe
      List("x", "y", "z", "null", "42", "null")
    elements(root, "seq").map(text) shouldBe List("1", "2", "3", "null")
  }

  "A service's config" should "take its profile's values over its defaults" in {
    val root = load("realistic/service.psi")
    List(
      "service.environment",
      "http.port",
      "http.timeouts.idle",
      "database.host",
      "database.url",
      "database.password",
      "database.pool.max",
      "database.pool.min",
      "database.health-query",
      "logging.level",
      "logging.pattern"
    ).map(at(root, _)) shouldBe List(
      "production",
      "443",
      "2m",
      "orders-db.prod.internal",
      "jdbc:postgresql://orders-db.prod.internal:5432/orders",
      "none",
      "64",
      "2",
      "SELECT 1\nFROM pg_catalog.pg_tables\nLIMIT 1",
      "WARN",
      "%d{ISO8601} %-5level [%thread] %logger{36} - %msg%n"
    )
    root("logging") match {
      case logging: Node.IMap[?] =>
        logging("loggers") match {
          case loggers: Node.IMap[?] => loggers.iterator.map(_.id.key).toList shouldBe
              List("org.apache.kafka", "com.zaxxer.hikari")
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
    elements(root, "features").size shouldBe 1
  }

  "A Spark job's config" should "build its paths by concatenation and keep its tags" in {
    val root = load("realistic/spark-job.psi")
    List("paths.output", "paths.checkpoint", "spark.master").map(at(root, _)) shouldBe
      List("s3a://warehouse/revenue/daily", "s3a://warehouse/revenue/_checkpoints/daily-revenue", "yarn")
    root("paths") match {
      case paths: Node.IMap[?] =>
        paths("input") match {
          case input: Node.ISeq[?] =>
            input.iterator.map(text).toList shouldBe
              List("s3a://warehouse/revenue/raw/orders", "s3a://warehouse/revenue/raw/refunds")
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
    root("alerts") match {
      case alerts: Node.IMap[?] =>
        alerts("on-failure") match {
          case channels: Node.ISeq[?] => channels.iterator.map(_.tags).toList shouldBe List(Set("email"), Set("slack"))
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
  }
}
