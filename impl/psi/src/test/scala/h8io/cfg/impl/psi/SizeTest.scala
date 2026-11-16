package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SizeTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def loader(nodes: Int, characters: Int): PsiLoader =
    Psi.builder
      .withSystemProperties(Map.empty)
      .withSizeLimit(nodes, characters)
      .build
      .fold(e => fail(e.toString), identity)

  /** `a0` a sequence of `width` scalars, each next key a map of `width` grafts of the one before. */
  private def fanOut(levels: Int, width: Int): String =
    (("a0: " + Seq.fill(width)("x").mkString("[", ", ", "]")) +: (1 to levels).map { i =>
      val grafts = (0 until width).map(k => "k" + k.toString + ": $" + "{a" + (i - 1).toString + "}")
      "a" + i.toString + " " + grafts.mkString("{ ", ", ", " }")
    }).mkString("\n")

  /** `a0` a scalar of `text`, each next key the one before concatenated twice. */
  private def doubling(links: Int, text: String): String =
    (("a0: " + text) +: (1 to links).map { i =>
      val before = "$" + "{a" + (i - 1).toString + "}"
      "a" + i.toString + ": \"" + before + before + "\""
    }).mkString("\n")

  "The size limit" should "be at least 0, of nodes and of characters alike" in {
    Psi.builder.withSizeLimit(0, 0).build.isRight shouldBe true
    for ((nodes, characters) <- List((-1, 0), (0, -1), (-1, -1)))
      Psi.builder.withSizeLimit(nodes, characters).build shouldBe
        Left(PsiBuildErrors(::(InvalidSizeLimit(nodes, characters), Nil)))
  }

  "Grafts" should "copy as many nodes into the tree as the limit, and no more, the first past it reported alone" in {
    // a0 holds 3 nodes, a1 grafts it twice, 6, and holds 7; a2 grafts a1 twice, 14, 20 in all; a3, 15 more, 35
    loader(20, 0).parse("s", fanOut(2, 2)).isRight shouldBe true
    loader(25, 0).parse("s", fanOut(3, 2)).left.map(_.errors) shouldBe
      Left(List(ResolutionTooLarge(at(4, 10), "a2", 25)))
  }

  they should "count the nodes they copy, not those the sources hold" in {
    loader(0, 0).parse("s", "a { b: 1, c: [2, 3] }").isRight shouldBe true
    loader(0, 0).parse("s", "a: 1\nb: $" + "{a}").left.map(_.errors) shouldBe
      Left(List(ResolutionTooLarge(at(2, 4), "a", 0)))
  }

  "Concatenations" should "build as many characters as the limit, and no more, the first past it reported alone" in {
    // a1 builds 8 characters, a2 16 more, a3 32 more
    loader(0, 8).parse("s", doubling(1, "xxxx")).isRight shouldBe true
    loader(0, 20).parse("s", doubling(3, "xxxx")).left.map(_.errors) shouldBe
      Left(List(ConcatenationTooLong(at(3, 5), 20)))
  }

  "A source of a few hundred bytes" should "fail on the default limits, not run out of memory" in {
    Psi.parse("s", fanOut(6, 10)).left.map(_.errors.map(_.getClass)) shouldBe Left(List(classOf[ResolutionTooLarge]))
    Psi.parse("s", doubling(25, "xxxxxxxxxx")).left.map(_.errors.map(_.getClass)) shouldBe
      Left(List(classOf[ConcatenationTooLong]))
  }
}
