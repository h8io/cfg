package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigObject, ConfigOrigin}
import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Entry, Path}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CfgMapImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return a CfgScalar" in {
    val obj = hocon"""scalar: 42"""
    val scalar = obj.get("scalar")
    inside(MapImpl(Path.Root, obj)("scalar")) { case Entry.Scalar(Path.Key("scalar"), "42", OriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a CfgNull" in {
    val obj = hocon"""scalar: null"""
    val scalar = obj.get("scalar")
    inside(MapImpl(Path.Root, obj)("scalar")) { case Entry.Null(Path.Key("scalar"), OriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a CfgNone" in {
    val obj = hocon"""scalar: 13"""
    inside(MapImpl(Path.Root, obj)("unexistent")) { case Entry.None(Path.Key("unexistent"), OriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs obj.origin
    }
  }

  it should "return a CfgSeq" in {
    val obj = hocon"""seq: [a, null, b, null, c, null, "null"]"""
    val list = obj.toConfig.getList("seq")
    inside(MapImpl(Path.Root, obj)("seq")) { case seq: Entry.Seq[?] =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = list.get(i).origin
        inside(value) {
          case Entry.Scalar(Path.Index(`i`), value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case Entry.Null(Path.Index(`i`), OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("a"), None, Some("b"), None, Some("c"), None, Some("null"))
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs list.origin }
    }
  }

  it should "return a CfgMap" in {
    val cfg = hocon"""map { a: null, b: c, null: "null" }"""
    val obj = cfg.toConfig.getObject("map")
    inside(MapImpl(Path.Root, cfg)("map")) { case map: Entry.Map[?] =>
      map.iterator.map { entry =>
        val expectedOrigin = obj.get(entry.path.key).origin
        inside(entry) {
          case Entry.Scalar(path, value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            path.key -> Some(value)
          case Entry.Null(path, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            path.key -> None
        }
      }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
      inside(map.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of entries" in {
    val obj = hocon"""a: null, b: c, null: "null""""
    MapImpl(Path.Root, obj).iterator.map { entry =>
      val expectedOrigin = obj.get(entry.path.key).origin
      inside(entry) {
        case Entry.Scalar(path, value, OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          path.key -> Some(value)
        case Entry.Null(path, OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          path.key -> None
      }
    }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
  }

  "isEmpty" should "return true" in {
    val obj = mock[ConfigObject]
    (obj.isEmpty _).expects().returns(true)
    MapImpl(Path.Root, obj).isEmpty shouldBe true
  }

  it should "return false" in {
    val obj = mock[ConfigObject]
    (obj.isEmpty _).expects().returns(false)
    MapImpl(Path.Root, obj).isEmpty shouldBe false
  }

  "size" should "return the same value as underlying.size" in {
    val obj = mock[ConfigObject]
    (obj.size _).expects().returns(17)
    MapImpl(Path.Root, obj).size shouldBe 17
  }

  "knownSize" should "return the same value as underlying.size" in {
    val obj = mock[ConfigObject]
    (obj.size _).expects().returns(42)
    MapImpl(Path.Root, obj).knownSize shouldBe 42
  }

  "origin" should "be a wrap on underlying origin object" in {
    val obj = mock[ConfigObject]
    val expectedOrigin = mock[ConfigOrigin]
    (obj.origin _).expects().returns(expectedOrigin)
    inside(MapImpl(Path.Root, obj).origin) { case OriginImpl(origin) =>
      origin should be theSameInstanceAs expectedOrigin
    }
  }
}
