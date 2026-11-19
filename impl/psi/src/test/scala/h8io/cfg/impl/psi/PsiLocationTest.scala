package h8io.cfg.impl.psi

import h8io.cfg.Location
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PsiLocationTest extends AnyFlatSpec with Matchers {
  private val app = SourceLocation("file:/etc/app/app.psi", 12, 9)
  private val base = SourceLocation("base.psi", 3, 7)

  "SourceLocation" should "render the source as named with its line and column" in {
    app.description shouldBe "file:/etc/app/app.psi:12:9"
  }

  "ReferenceLocation" should "render the reference site first and the origin in parenthese" in {
    ReferenceLocation(app, "db.url", base).description shouldBe
      "file:/etc/app/app.psi:12:9 (${db.url} from base.psi:3:7)"
  }

  it should "render a chain of substitutions recursively" in {
    val chain = ReferenceLocation(
      SourceLocation("app.psi", 1, 4),
      "b",
      ReferenceLocation(SourceLocation("app.psi", 2, 4), "c", SourceLocation("app.psi", 3, 4)))
    chain.description shouldBe "app.psi:1:4 (${b} from app.psi:2:4 (${c} from app.psi:3:4))"
  }

  it should "render an origin of any kind through its description" in {
    val foreign = new Location { def description: String = "elsewhere" }
    ReferenceLocation(app, "x", foreign).description shouldBe "file:/etc/app/app.psi:12:9 (${x} from elsewhere)"
  }

  "ConcatenationLocation" should "list its parts without their own site" in {
    ConcatenationLocation(
      SourceLocation("app.psi", 12, 9),
      ::(
        ReferenceLocation(SourceLocation("app.psi", 12, 17), "host", base),
        ReferenceLocation(SourceLocation("app.psi", 12, 25), "port", SystemPropertyLocation("db.port")) :: Nil
      )
    ).description shouldBe "app.psi:12:9 (${host} from base.psi:3:7, ${port} from system property db.port)"
  }

  it should "render a single part" in {
    ConcatenationLocation(app, ::(ReferenceLocation(app, "a", base), Nil)).description shouldBe
      "file:/etc/app/app.psi:12:9 (${a} from base.psi:3:7)"
  }

  "MergedLocation" should "list the earlier sites after the last one, the latest first" in {
    MergedLocation(app, ::(SourceLocation("app.psi", 7, 1), base :: Nil)).description shouldBe
      "file:/etc/app/app.psi:12:9 (also app.psi:7:1, base.psi:3:7)"
    MergedLocation(app, ::(base, Nil)).description shouldBe "file:/etc/app/app.psi:12:9 (also base.psi:3:7)"
  }

  "A location sharing those it refers to" should "be cut off at the limit, and hash in time linear in its size" in {
    // each link concatenates the one before twice: a description of 2^40 parts
    val link = (1 to 40).foldLeft[PsiLocation](SourceLocation("s", 1, 1)) { (before, i) =>
      val part = ReferenceLocation(SourceLocation("s", i + 1, 5), "a", before)
      ConcatenationLocation(SourceLocation("s", i + 1, 4), ::(part, part :: Nil))
    }
    val description = link.description
    description.length shouldBe PsiLocation.DescriptionLimit + 1
    description should startWith("s:41:4 (${a} from s:40:4 (${a} from")
    description should endWith("…")
    link.hashCode shouldBe link.hashCode
    MergedLocation(link, ::(link, Nil)).description.length shouldBe PsiLocation.DescriptionLimit + 1
    MergedLocation(link, ::(link, Nil)).hashCode shouldBe MergedLocation(link, ::(link, Nil)).hashCode
    DirectiveLocation(app, "d", link).hashCode shouldBe DirectiveLocation(app, "d", link).hashCode
  }

  it should "show itself as a case class does, cut off at the limit" in {
    val link = (1 to 40).foldLeft[PsiLocation](SourceLocation("s", 1, 1)) { (before, i) =>
      val part = ReferenceLocation(SourceLocation("s", i + 1, 5), "a", before)
      ConcatenationLocation(SourceLocation("s", i + 1, 4), ::(part, part :: Nil))
    }
    val shown = link.toString
    shown.length shouldBe PsiLocation.DescriptionLimit + 1
    shown should startWith(
      "ConcatenationLocation(SourceLocation(s,41,4),List(ReferenceLocation(SourceLocation(s,41,5),a,Concatenation")
    shown should endWith("…")
    val small = ConcatenationLocation(app, ::(ReferenceLocation(app, "a", base), Nil))
    small.toString shouldBe
      "ConcatenationLocation(SourceLocation(file:/etc/app/app.psi,12,9),List(ReferenceLocation(SourceLocation(" +
      "file:/etc/app/app.psi,12,9),a,SourceLocation(base.psi,3,7))))"
    MergedLocation(app, ::(base, NoSourceLocation :: Nil)).toString shouldBe
      "MergedLocation(SourceLocation(file:/etc/app/app.psi,12,9),List(SourceLocation(base.psi,3,7),NoSourceLocation))"
    ReferenceLocation(app, "a", base).toString shouldBe
      "ReferenceLocation(SourceLocation(file:/etc/app/app.psi,12,9),a,SourceLocation(base.psi,3,7))"
    DirectiveLocation(app, "d", base).toString shouldBe
      "DirectiveLocation(SourceLocation(file:/etc/app/app.psi,12,9),d,SourceLocation(base.psi,3,7))"
  }

  it should "be whole up to the limit" in {
    val long = SourceLocation("x" * (PsiLocation.DescriptionLimit - 4), 1, 1)
    long.description.length shouldBe PsiLocation.DescriptionLimit
    MergedLocation(long, ::(base, Nil)).description shouldBe long.description + "…"
  }

  "NoSourceLocation" should "say there was no source" in {
    NoSourceLocation.description shouldBe "no source"
  }

  "order" should "put a location by its source among those given, then by line and column, and last elsewhere" in {
    val sources = List("a.psi", "b.psi")
    SourceLocation("b.psi", 2, 3).order(sources) shouldBe ((1, 2, 3))
    SourceLocation("c.psi", 2, 3).order(sources) shouldBe ((2, 2, 3))
  }

  "SystemPropertyLocation" should "name the property" in {
    SystemPropertyLocation("db.url").description shouldBe "system property db.url"
  }

  "EnvironmentLocation" should "name the variable" in {
    EnvironmentLocation("HOME").description shouldBe "environment variable HOME"
  }

  /** A chain of `links` concatenations, each referring twice to the one before, built apart from any other. */
  private def doubling(links: Int, bottom: PsiLocation): PsiLocation = (1 to links).foldLeft(bottom) { (before, i) =>
    val part = ReferenceLocation(SourceLocation("s", i + 1, 5), "a", before)
    ConcatenationLocation(SourceLocation("s", i + 1, 4), ::(part, part :: Nil))
  }

  "A composite location" should "equal another built the same way apart, comparing each pair of locations once" in {
    doubling(100, base) shouldEqual doubling(100, base)
    doubling(100, base) should not equal doubling(100, app)
    val foreign = new Location { def description: String = "elsewhere" }
    ReferenceLocation(app, "a", foreign) shouldEqual ReferenceLocation(app, "a", foreign)
    ReferenceLocation(app, "a", foreign) should not equal ReferenceLocation(app, "a",
      new Location {
        def description: String = "elsewhere"
      })
    ReferenceLocation(app, "a", base) should not equal ReferenceLocation(app, "b", base)
  }

  it should "equal another of its own kind alone" in {
    val reference = ReferenceLocation(app, "a", base)
    val concatenation = ConcatenationLocation(app, ::(reference, Nil))
    val merged = MergedLocation(app, ::(base, Nil))
    val directive = DirectiveLocation(app, "d", base)
    val all = List(reference, concatenation, merged, directive)
    for {
      a <- all
      b <- all
    } (a == b) shouldBe (a eq b)
    MergedLocation(doubling(30, app), ::(doubling(30, base), Nil)) shouldEqual
      MergedLocation(doubling(30, app), ::(doubling(30, base), Nil))
    merged should not equal MergedLocation(app, ::(base, base :: Nil))
    DirectiveLocation(app, "d", doubling(30, base)) shouldEqual DirectiveLocation(app, "d", doubling(30, base))
    directive should not equal DirectiveLocation(app, "e", base)
    ReferenceLocation(app, "a", reference) should not equal ReferenceLocation(app, "a", concatenation)
    ReferenceLocation(app, "a", base) should not equal ReferenceLocation(app, "a", reference)
    ReferenceLocation(app, "a", reference) should not equal ReferenceLocation(app, "a", base)
    reference should not equal "a"
  }

  "Two loads of one source" should "equal, however many paths lead to the locations they share" in {
    val text =
      ("a0: \"\"" +: (1 to 100).map(i =>
        "a" + i.toString + ": \"$" + "{a" + (i - 1).toString + "}$" +
          "{a" + (i - 1).toString + "}\"")).mkString("\n")
    Psi.parse("s", text).isRight shouldBe true
    Psi.parse("s", text) shouldEqual Psi.parse("s", text)
  }
}
