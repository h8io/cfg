package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigObject, ConfigOrigin, ConfigRenderOptions}
import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class MapImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return a Node.Scalar object" in {
    val obj = hocon"""scalar: 42"""
    val scalar = obj.get("scalar")
    inside(MapImpl(Id.Root, obj)("scalar")) {
      case Node.Scalar(Id.Key("scalar", Id.Root), "42", LocationImpl(scalarOrigin)) =>
        scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a Node.Null object" in {
    val obj = hocon"""scalar: null"""
    val scalar = obj.get("scalar")
    inside(MapImpl(Id.Root, obj)("scalar")) {
      case Node.Null(Id.Key("scalar", Id.Root), LocationImpl(scalarOrigin)) =>
        scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a Node.None object" in {
    val map = MapImpl(Id.Root, hocon"""scalar: 13""")
    map("unexistent") should matchPattern { case Node.None(Id.Key("unexistent", Id.Root), `map`) => }
  }

  it should "return a Node.Seq object" in {
    val obj = hocon"""seq: [a, null, b, null, c, null, "null"]"""
    val list = obj.toConfig.getList("seq")
    inside(MapImpl(Id.Root, obj)("seq")) { case seq: Node.Seq[Id.Key] =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = list.get(i).origin
        val id = seq.id
        inside(value) {
          case Node.Scalar(Id.Index(`i`, `id`), value, LocationImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case Node.Null(Id.Index(`i`, `id`), LocationImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("a"), None, Some("b"), None, Some("c"), None, Some("null"))
      inside(seq.location) { case LocationImpl(origin) => origin should be theSameInstanceAs list.origin }
    }
  }

  it should "return a Node.Map object" in {
    val cfg = hocon"""map { a: null, b: c, null: "null" }"""
    val obj = cfg.toConfig.getObject("map")
    inside(MapImpl(Id.Root, cfg)("map")) { case map: Node.Map[Id.Key] =>
      map.iterator.map { node =>
        inside(node) {
          case Node.Scalar(Id.Key(key, Id.Key("map", Id.Root)), value, LocationImpl(origin)) =>
            origin should be theSameInstanceAs obj.get(key).origin
            key -> Some(value)
          case Node.Null(Id.Key(key, Id.Key("map", Id.Root)), LocationImpl(origin)) =>
            origin should be theSameInstanceAs obj.get(key).origin
            key -> None
        }
      }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
      inside(map.location) { case LocationImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of nodes" in {
    val obj = hocon"""a: null, b: c, null: "null""""
    MapImpl(Id.Root, obj).iterator.map { node =>
      inside(node) {
        case Node.Scalar(Id.Key(key, Id.Root), value, LocationImpl(origin)) =>
          origin should be theSameInstanceAs obj.get(key).origin
          key -> Some(value)
        case Node.Null(Id.Key(key, Id.Root), LocationImpl(origin)) =>
          origin should be theSameInstanceAs obj.get(key).origin
          key -> None
      }
    }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
  }

  "size" should "return the same value as underlying.size" in {
    val obj = mock[ConfigObject]
    (obj.size _).expects().returns(17)
    MapImpl(Id.Root, obj).size shouldBe 17
  }

  "location" should "be a wrap on underlying origin object" in {
    val obj = mock[ConfigObject]
    val expectedOrigin = mock[ConfigOrigin]
    (obj.origin _).expects().returns(expectedOrigin)
    inside(MapImpl(Id.Root, obj).location) { case LocationImpl(origin) =>
      origin should be theSameInstanceAs expectedOrigin
    }
  }

  "toString" should "delegate call to underlying.render" in {
    val obj = mock[ConfigObject]
    val expected = Random.nextString(16)
    (obj.render(_: ConfigRenderOptions)).expects(RenderOptions).returns(expected)
    MapImpl(Id.Root, obj).toString should be theSameInstanceAs expected
  }
}
