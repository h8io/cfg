package h8io.cfg.impl.psi

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.charset.StandardCharsets
import scala.io.Source as IO

class HardeningTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private def text(name: String): String = {
    val source = IO.fromInputStream(getClass.getResourceAsStream("/" + name), StandardCharsets.UTF_8.name)
    try source.mkString
    finally source.close()
  }

  /** Loads `text`, answering whether it loaded; throws only if the loader does. */
  private def loads(text: String): Boolean = Psi.parse("s", text).isRight

  "A large source" should "load" in {
    val n = 200000
    val source = (1 to n).map(i => "k" + i.toString + ": v" + i.toString).mkString("\n")
    Psi.parse("s", source).map(root => (root.size, root("k" + n.toString).toString)) match {
      case Right((fields, last)) =>
        fields shouldBe n
        last should include("v" + n.toString)
      case Left(errors) => fail(errors.toString)
    }
    val long = "x" * 10000000
    Psi.parse("s", "a: \"" + long + "\"").map(_.size) shouldBe Right(1)
  }

  "A source cut off anywhere" should "load or give errors, never throw" in {
    val texts = List("realistic/service.psi", "realistic/spark-job.psi", "ported/wrap-test.psi", "base.psi").map(text)
    val cut = texts.flatMap(text => (0 to text.length).map(text.substring(0, _)))
    cut.count(loads) should be > 0
  }

  "A source that ends inside a construct" should "give errors, never throw" in {
    val unterminated =
      List("a: \"x", "a: \"\"\"", "a: 'x", "a: '''", "`a", "/* a", "/** a", "a: ${", "a: ${b", "#x(", "a: @", "#", "a:")
    unterminated.filter(loads) shouldBe Nil
  }

  "Any text of Psi's own characters" should "load or give errors, never throw" in {
    val chars = Gen.oneOf("{}[]():,.=@#$?\"'`\\/*<>+-_ \n\t\rab01é😀 ​".toVector.map(_.toString) :+ "😀")
    forAll(Gen.listOf(chars).map(_.mkString), minSuccessful(1000), sizeRange(200)) { text =>
      val _ = loads(text)
    }
  }

  "Any arrangement of Psi's tokens" should "load or give errors, never throw" in {
    val dollar = "$"
    val tokens = Gen.oneOf(
      Vector(
        "a",
        "b",
        "a.b",
        ":",
        ":=",
        " ",
        "\n",
        ",",
        "{",
        "}",
        "[",
        "]",
        "(",
        ")",
        dollar + "{a}",
        dollar + "{?b.c}",
        dollar + "{a[0]}",
        "\"x" + dollar + "{a}y\"",
        "'r'",
        "@t ",
        "null",
        "#include(file: \"none\", optional: true)",
        "#x",
        "#x(1)",
        "\"\"\"<\n  t\n  \"\"\"",
        "'''\nu\n'''",
        "/* c */",
        "// c\n",
        "/** d */",
        "`k`",
        "\\n",
        "=",
        "1"
      ))
    forAll(Gen.listOf(tokens).map(_.mkString), minSuccessful(1000), sizeRange(100)) { text =>
      val _ = loads(text)
    }
  }
}
