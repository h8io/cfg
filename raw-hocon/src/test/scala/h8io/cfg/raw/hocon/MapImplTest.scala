package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigObject, ConfigOrigin}
import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MapImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return a Node.Scalar object" in {
    val obj = hocon"""scalar: 42"""
    val scalar = obj.get("scalar")
    inside(MapImpl(Id.Root, obj)("scalar")) {
      case Node.Scalar(Id.Key("scalar"), "42", OriginImpl(scalarOrigin)) =>
        scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a Node.Null object" in {
    val obj = hocon"""scalar: null"""
    val scalar = obj.get("scalar")
    inside(MapImpl(Id.Root, obj)("scalar")) {
      case Node.Null(Id.Key("scalar"), OriginImpl(scalarOrigin)) =>
        scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a Node.None object" in {
    val obj = hocon"""scalar: 13"""
    inside(MapImpl(Id.Root, obj)("unexistent")) {
      case Node.None(Id.Key("unexistent"), OriginImpl(scalarOrigin)) =>
        scalarOrigin should be theSameInstanceAs obj.origin
    }
  }

  it should "return a Node.Seq object" in {
    val obj = hocon"""seq: [a, null, b, null, c, null, "null"]"""
    val list = obj.toConfig.getList("seq")
    inside(MapImpl(Id.Root, obj)("seq")) { case seq: Node.Seq[?] =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = list.get(i).origin
        inside(value) {
          case Node.Scalar(Id.Index(`i`), value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case Node.Null(Id.Index(`i`), OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("a"), None, Some("b"), None, Some("c"), None, Some("null"))
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs list.origin }
    }
  }

  it should "return a Node.Map object" in {
    val cfg = hocon"""map { a: null, b: c, null: "null" }"""
    val obj = cfg.toConfig.getObject("map")
    inside(MapImpl(Id.Root, cfg)("map")) { case map: Node.Map[?] =>
      map.iterator.map { node =>
        val expectedOrigin = obj.get(node.id.key).origin
        inside(node) {
          case Node.Scalar(id, value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            id.key -> Some(value)
          case Node.Null(id, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            id.key -> None
        }
      }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
      inside(map.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of nodes" in {
    val obj = hocon"""a: null, b: c, null: "null""""
    MapImpl(Id.Root, obj).iterator.map { node =>
      val expectedOrigin = obj.get(node.id.key).origin
      inside(node) {
        case Node.Scalar(id, value, OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          id.key -> Some(value)
        case Node.Null(id, OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          id.key -> None
      }
    }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
  }

  "isEmpty" should "return true" in {
    val obj = mock[ConfigObject]
    (obj.isEmpty _).expects().returns(true)
    MapImpl(Id.Root, obj).isEmpty shouldBe true
  }

  it should "return false" in {
    val obj = mock[ConfigObject]
    (obj.isEmpty _).expects().returns(false)
    MapImpl(Id.Root, obj).isEmpty shouldBe false
  }

  "size" should "return the same value as underlying.size" in {
    val obj = mock[ConfigObject]
    (obj.size _).expects().returns(17)
    MapImpl(Id.Root, obj).size shouldBe 17
  }

  "knownSize" should "return the same value as underlying.size" in {
    val obj = mock[ConfigObject]
    (obj.size _).expects().returns(42)
    MapImpl(Id.Root, obj).knownSize shouldBe 42
  }

  "origin" should "be a wrap on underlying origin object" in {
    val obj = mock[ConfigObject]
    val expectedOrigin = mock[ConfigOrigin]
    (obj.origin _).expects().returns(expectedOrigin)
    inside(MapImpl(Id.Root, obj).origin) { case OriginImpl(origin) =>
      origin should be theSameInstanceAs expectedOrigin
    }
  }
}
