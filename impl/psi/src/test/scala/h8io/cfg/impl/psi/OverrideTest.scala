package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverrideTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def loaded(text: String, properties: Map[String, String], env: Map[String, String]): Node.IMap[Id.Root] =
    Psi.load(Right(Source("s", text)) :: Nil, properties, env).fold(errors => fail(errors.toString), identity)

  private def scalar(id: Id, value: String, location: PsiLocation, tags: String*): Node.IScalar[Id] =
    Node.IScalar(id, tags.toSet, value, location)

  private val db = Id.Key("db", Id.Root)

  "A system property" should "override the value at its path, keeping the tags written" in {
    val root = loaded("db { url: @secret x, port: 1 }", Map("db.url" -> "jdbc:p"), Map.empty)
    root("db") match {
      case map: Node.IMap[?] =>
        map("url") shouldBe scalar(Id.Key("url", db), "jdbc:p", SystemPropertyLocation("db.url"), "secret")
        map("port") shouldBe scalar(Id.Key("port", db), "1", at(1, 28))
      case other => fail(other.toString)
    }
  }

  it should "win over the environment" in {
    loaded("db.url: x", Map("db.url" -> "p"), Map("DB_URL" -> "e"))("db") match {
      case map: Node.IMap[?] => map("url") shouldBe scalar(Id.Key("url", db), "p", SystemPropertyLocation("db.url"))
      case other => fail(other.toString)
    }
  }

  "An environment variable" should "override the value at the path its name is made of" in {
    val root = loaded("pool { max-size: 1 }\nn: null\ns: ${t}\nc: \"${t}x\"", Map.empty,
      Map("POOL_MAX_SIZE" -> "4", "N" -> "n", "S" -> "s", "C" -> "c"))
    root("pool") match {
      case map: Node.IMap[?] =>
        map("max-size") shouldBe scalar(Id.Key("max-size", Id.Key("pool", Id.Root)), "4", EnvLocation("POOL_MAX_SIZE"))
      case other => fail(other.toString)
    }
    root("n") shouldBe scalar(Id.Key("n", Id.Root), "n", EnvLocation("N"))
    root("s") shouldBe scalar(Id.Key("s", Id.Root), "s", EnvLocation("S"))
    root("c") shouldBe scalar(Id.Key("c", Id.Root), "c", EnvLocation("C"))
  }

  "An override" should "neither add a key nor replace a container, nor reach into a sequence" in {
    val root = loaded("m { k: 1 }\nl: [1, { k: 2 }]", Map("x" -> "1", "m" -> "2", "l" -> "3", "l.k" -> "4"),
      Map("X" -> "1", "M" -> "2", "L_K" -> "4"))
    root.size shouldBe 2
    root("x") shouldBe Node.None(Id.Key("x", Id.Root))
    root("m") shouldBe a[Node.IMap[?]]
    root("l") match {
      case seq: Node.ISeq[?] => seq.size shouldBe 2
      case other => fail(other.toString)
    }
  }

  it should "be what a substitution reads" in {
    loaded("db.url: x\nu: ${db.url}", Map.empty, Map("DB_URL" -> "e"))("u") shouldBe
      scalar(Id.Key("u", Id.Root), "e", ReferenceLocation(at(2, 4), "db.url", EnvLocation("DB_URL")))
  }

  "The name of a variable" should "be the keys in upper case, a dash as an underscore, joined by underscores" in {
    Override.env(List("db", "max-size")) shouldBe "DB_MAX_SIZE"
    Override.env(List("ı")) shouldBe "I"
  }
}
