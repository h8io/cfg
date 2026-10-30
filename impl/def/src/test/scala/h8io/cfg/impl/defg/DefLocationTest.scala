package h8io.cfg.impl.defg

import h8io.cfg.Location
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DefLocationTest extends AnyFlatSpec with Matchers {
  private val app = SourceLocation("file:/etc/app/app.def", 12, 9)
  private val base = SourceLocation("base.def", 3, 7)

  "SourceLocation" should "render the source as named with its line and column" in {
    app.description shouldBe "file:/etc/app/app.def:12:9"
  }

  "ReferenceLocation" should "render the reference site first and the origin in parentheses" in {
    ReferenceLocation(app, "db.url", base).description shouldBe
      s"file:/etc/app/app.def:12:9 ($${db.url} from base.def:3:7)"
  }

  it should "render a chain of substitutions recursively" in {
    val chain = ReferenceLocation(
      SourceLocation("app.def", 1, 4),
      "b",
      ReferenceLocation(SourceLocation("app.def", 2, 4), "c", SourceLocation("app.def", 3, 4)))
    chain.description shouldBe s"app.def:1:4 ($${b} from app.def:2:4 ($${c} from app.def:3:4))"
  }

  it should "render an origin of any kind through its description" in {
    val foreign = new Location { def description: String = "elsewhere" }
    ReferenceLocation(app, "x", foreign).description shouldBe s"file:/etc/app/app.def:12:9 ($${x} from elsewhere)"
  }

  "ConcatenationLocation" should "list its parts without their own sites" in {
    ConcatenationLocation(
      SourceLocation("app.def", 12, 9),
      ::(
        ReferenceLocation(SourceLocation("app.def", 12, 17), "host", base),
        ReferenceLocation(SourceLocation("app.def", 12, 25), "port", EnvLocation("DB_PORT")) :: Nil
      )
    ).description shouldBe s"app.def:12:9 ($${host} from base.def:3:7, $${port} from env DB_PORT)"
  }

  it should "render a single part" in {
    ConcatenationLocation(app, ::(ReferenceLocation(app, "a", base), Nil)).description shouldBe
      s"file:/etc/app/app.def:12:9 ($${a} from base.def:3:7)"
  }

  "SystemPropertyLocation" should "name the property" in {
    SystemPropertyLocation("db.url").description shouldBe "system property db.url"
  }

  "EnvLocation" should "name the variable" in {
    EnvLocation("DB_PORT").description shouldBe "env DB_PORT"
  }
}
