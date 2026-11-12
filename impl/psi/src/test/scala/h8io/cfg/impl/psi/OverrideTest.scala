package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverrideTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def loaded(text: String, properties: (String, String)*): Node.IMap[Id.Root] =
    Psi.load(Right(Source("s", text)) :: Nil, properties.toMap).fold(errors => fail(errors.toString), identity)

  private def scalar(id: Id, value: String, location: PsiLocation, tags: String*): Node.IScalar[Id] =
    Node.IScalar(id, tags.toSet, value, location)

  private def keyed(names: String*): Id.Key =
    names.tail.foldLeft(Id.Key(names.head, Id.Root))((parent, name) => Id.Key(name, parent))

  "A system property" should "override the value at its path, keeping the tags written" in {
    loaded("db { url: @secret x, port: 1 }", "db.url" -> "jdbc:p")("db") match {
      case map: Node.IMap[?] =>
        map("url") shouldBe scalar(keyed("db", "url"), "jdbc:p", SystemPropertyLocation("db.url"), "secret")
        map("port") shouldBe scalar(keyed("db", "port"), "1", at(1, 28))
      case other => fail(other.toString)
    }
  }

  it should "override null, a substitution and a concatenation, whose references are then never resolved" in {
    val root = loaded("n: null\ns: ${t}\nc: \"${t}x\"", "n" -> "1", "s" -> "2", "c" -> "3")
    root("n") shouldBe scalar(keyed("n"), "1", SystemPropertyLocation("n"))
    root("s") shouldBe scalar(keyed("s"), "2", SystemPropertyLocation("s"))
    root("c") shouldBe scalar(keyed("c"), "3", SystemPropertyLocation("c"))
  }

  it should "neither add a key nor replace a container, nor reach into a sequence" in {
    val root = loaded("m { k: 1 }\nl: [1, { k: 2 }]", "x" -> "1", "m" -> "2", "l" -> "3", "l.k" -> "4")
    root.size shouldBe 2
    root("x") shouldBe Node.None(keyed("x"))
    root("m") match {
      case map: Node.IMap[?] => map("k") shouldBe scalar(keyed("m", "k"), "1", at(1, 8))
      case other => fail(other.toString)
    }
    root("l") match {
      case seq: Node.ISeq[?] => seq.location shouldBe at(2, 4)
      case other => fail(other.toString)
    }
  }

  it should "be what a substitution reads" in {
    loaded("db.url: x\nu: ${db.url}", "db.url" -> "p")("u") shouldBe
      scalar(keyed("u"), "p", ReferenceLocation(at(2, 4), "db.url", SystemPropertyLocation("db.url")))
  }
}
