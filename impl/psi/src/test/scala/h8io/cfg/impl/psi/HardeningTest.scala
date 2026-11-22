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

  "A line of many formatters" should "load in time linear in its length" in {
    // each formatter copied the rest of its line, which took minutes on this one
    val n = 200000
    Psi.parse("s", "h: [a]\nx: \"" + "${h; join(',')}" * n + "\"").map(_("x")) match {
      case Right(h8io.cfg.Node.IScalar(_, _, value, _)) => value shouldBe "a" * n
      case other => fail(other.toString)
    }
  }

  "A source of many broken fields" should "give its errors in time linear in its length" in {
    // the errors the parser read past were dropped from a list, walking it from the head, which took seconds here
    val n = 200000
    Psi.parse("s", "a:\n  1: x\n" * n).left.map(_.errors.size) shouldBe Left(n)
  }

  "A sequence looked into at every index" should "load in time linear in its length" in {
    // each lookup walked the sequence from its first element, which took minutes here
    val n = 200000
    val lookups = (0 until n).map(i => "r" + i.toString + ": $s[" + i.toString + "]").mkString("\n")
    for (sequence <-
        List("s: [" + (0 until n).mkString(", ") + "]", "t: [a]\ns: [$t, " + (1 until n).mkString(", ") + "]"))
      Psi.parse("s", sequence + "\n" + lookups).map(root => (root.size, root("r" + (n - 1).toString).toString)) match {
        case Right((size, last)) =>
          size should be > n
          last should include((n - 1).toString)
        case Left(errors) => fail(errors.toString)
      }
  }

  "A long line with a character beyond Latin-1" should "load as fast as one of Latin-1" in {
    // a column counted from the start of its line for every token took minutes on this line
    val n = 500000
    val line = "x: [ψ, 😀" + ", 1" * n + "]"
    Psi.parse("s", line).map(root => root("x").toString) match {
      case Right(x) => x shouldBe "- ψ\n- 😀" + "\n- 1" * n
      case Left(errors) => fail(errors.toString)
    }
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
        "#include(file: \"none\", optional: true)*",
        "#x*",
        "#x(1)*",
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
