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
    val a = SourceLocation("a.psi", 4, 5)
    ReferenceLocation(a, "x", base).order(sources) shouldBe ((0, 4, 5))
    ConcatenationLocation(a, ::(ReferenceLocation(a, "x", base), Nil)).order(sources) shouldBe ((0, 4, 5))
    MergedLocation(a, ::(SourceLocation("b.psi", 1, 1), Nil)).order(sources) shouldBe ((0, 4, 5))
    NoSourceLocation.order(sources) shouldBe ((2, 0, 0))
    SystemPropertyLocation("p").order(sources) shouldBe ((2, 0, 0))
    EnvironmentLocation("E").order(sources) shouldBe ((2, 0, 0))
  }

  "SystemPropertyLocation" should "name the property" in {
    SystemPropertyLocation("db.url").description shouldBe "system property db.url"
  }

  "EnvironmentLocation" should "name the variable, and stand in no source" in {
    EnvironmentLocation("HOME").description shouldBe "environment variable HOME"
    EnvironmentLocation("HOME").anchor shouldBe None
  }

}
