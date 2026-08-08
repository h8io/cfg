package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MapImplTest extends AnyFlatSpec with Matchers with Inside {
  "apply" should "return a Node.Scalar object" in
    inside(MapImpl(Id.Root, yaml"scalar: 42")("scalar")) {
      case Node.Scalar(Id.Key("scalar", Id.Root), None, "42", location) =>
        location.description shouldBe s"$Label: 1:9"
    }

  it should "return a Node.Null object" in {
    MapImpl(Id.Root, yaml"scalar: ~")("scalar") should matchPattern {
      case Node.Null(Id.Key("scalar", Id.Root), None, _) =>
    }
  }

  it should "return a Node.None object" in {
    val map = MapImpl(Id.Root, yaml"scalar: 13")
    map("unexistent") should matchPattern { case Node.None(Id.Key("unexistent", Id.Root), `map`) => }
  }

  it should "return a Node.Seq object" in
    inside(MapImpl(Id.Root, yaml"seq: [a, ~, b]")("seq")) { case seq: Node.ISeq[Id.Key] =>
      val id = seq.id
      seq.iterator.map {
        inside(_) {
          case Node.Scalar(Id.Index(_, `id`), None, scalar, _) => Some(scalar)
          case Node.Null(Id.Index(_, `id`), None, _) => None
        }
      }.toList should contain theSameElementsInOrderAs List(Some("a"), None, Some("b"))
    }

  it should "return a Node.Map object" in
    inside(MapImpl(Id.Root, yaml"map: {a: ~, b: c}")("map")) { case map: Node.IMap[Id.Key] =>
      map.iterator.map {
        inside(_) {
          case Node.Scalar(Id.Key(key, Id.Key("map", Id.Root)), None, scalar, _) => key -> Some(scalar)
          case Node.Null(Id.Key(key, Id.Key("map", Id.Root)), None, _) => key -> None
        }
      }.toList should contain theSameElementsInOrderAs List("a" -> None, "b" -> Some("c"))
    }

  it should "return the last of the entries sharing a key" in {
    MapImpl(Id.Root,
      yaml"""
dup: 1
other: x
dup: 2
""")("dup") should matchPattern { case Node.Scalar(Id.Key("dup", Id.Root), None, "2", _) => }
  }

  "tag" should "return None when the source carries no tag" in {
    MapImpl(Id.Root, yaml"a: 1").tag shouldBe None
  }

  it should "return the tag written in the source" in {
    MapImpl(Id.Root, yaml"!postgres {a: 1}").tag shouldBe Some("!postgres")
  }

  "iterator" should "return a correct sequence of nodes" in {
    MapImpl(Id.Root,
      yaml"""
a: ~
b: c
"null": "null"
""").iterator.map {
      inside(_) {
        case Node.Scalar(Id.Key(key, Id.Root), None, scalar, _) => key -> Some(scalar)
        case Node.Null(Id.Key(key, Id.Root), None, _) => key -> None
      }
    }.toList should contain theSameElementsInOrderAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
  }

  it should "collapse the entries sharing a key onto the last one" in {
    MapImpl(Id.Root,
      yaml"""
dup: 1
other: x
dup: 2
""").iterator.map {
      inside(_) { case Node.Scalar(Id.Key(key, Id.Root), None, scalar, _) => key -> scalar }
    }.toList should contain theSameElementsInOrderAs List("other" -> "x", "dup" -> "2")
  }

  "size" should "count the keys rather than the entries" in {
    MapImpl(Id.Root, yaml"{a: 1, b: 2, c: 3}").size shouldBe 3
    MapImpl(Id.Root, yaml"{a: 1, a: 2, a: 3}").size shouldBe 1
    MapImpl(Id.Root, emptyMapping) shouldBe empty
  }

  "location" should "be the position of the mapping in the source" in {
    MapImpl(Id.Root,
      yaml"""
a: 1
""").location.description shouldBe s"$Label: 2:1"
  }

  "operator -" should "return a map without a specified key" in {
    val map = MapImpl(Id.Key("", Id.Root), yaml"{scalar: 1, seq: [c, d], map: {x: 12}}")
    inside(map - "scalar") { case result: MapImpl[?] =>
      result.location shouldBe map.location
      result.iterator.toList should contain theSameElementsInOrderAs List(map("seq"), map("map"))
    }
    inside(map - "map") { case result: MapImpl[?] =>
      result.iterator.toList should contain theSameElementsInOrderAs List(map("scalar"), map("seq"))
    }
  }

  it should "keep the tag of the source mapping" in {
    (MapImpl(Id.Root, yaml"!postgres {a: 1, b: 2}") - "a").tag shouldBe Some("!postgres")
    (MapImpl(Id.Root, yaml"{a: 1, b: 2}") - "a").tag shouldBe None
  }

  it should "return the existent node if key does not exist" in {
    val map = MapImpl(Id.Root, yaml"scalar: 1")
    map - "unexistent" should be theSameInstanceAs map
  }

  "toString" should "render the mapping back to YAML" in {
    MapImpl(Id.Root, yaml"{a: 1, b: [c, d]}").toString shouldBe "{a: 1, b: [c, d]}\n"
  }

  it should "render the entries sharing a key only once" in {
    MapImpl(Id.Root, yaml"{a: 1, a: 2}").toString shouldBe "{a: 2}\n"
  }
}
