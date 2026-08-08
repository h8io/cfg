package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.exceptions.YamlEngineException
import org.snakeyaml.engine.v2.nodes.{AnchorNode, MappingNode}

class WrapTest extends AnyFlatSpec with Matchers with Inside {
  private val config = yaml"""
map: {a: x, b: y}
seq: [1, 2, 3]
scalar: 42
nothing: ~
tagged-map: !postgres {a: x}
tagged-seq: !identifiers [1]
tagged-scalar: !!int 42
tagged-nothing: !!null ~
"""

  "wrap" should "create a Node.Map object" in {
    val value = valueOf(config, "map")
    val id = Id.Index(42, Id.Root)
    inside(wrap(id, value)) { case map: Node.IMap[Id.Index] =>
      map should be(MapImpl(id, value.asInstanceOf[MappingNode]))
      map.tag shouldBe None
      map.iterator.map {
        inside(_) { case Node.Scalar(Id.Key(key, `id`), None, scalar, _) => key -> scalar }
      }.toList should contain theSameElementsInOrderAs List("a" -> "x", "b" -> "y")
      map.location.description shouldBe s"$Label: 2:6"
    }
  }

  it should "create a Node.Seq object" in {
    val value = valueOf(config, "seq")
    inside(wrap(Id.Root, value)) { case seq: Node.ISeq[Id.Root] =>
      seq.iterator.zipWithIndex.map { case (node, i) =>
        inside(node) { case Node.Scalar(Id.Index(`i`, Id.Root), None, scalar, _) => scalar }
      }.toList should contain theSameElementsInOrderAs List("1", "2", "3")
      seq.tag shouldBe None
      seq.location.description shouldBe s"$Label: 3:6"
    }
  }

  it should "create a Node.Scalar object" in
    inside(wrap(Id.Root, valueOf(config, "scalar"))) { case Node.Scalar(Id.Root, None, "42", location) =>
      location.description shouldBe s"$Label: 4:9"
    }

  it should "create a Node.Null object" in
    inside(wrap(Id.Root, valueOf(config, "nothing"))) { case Node.Null(Id.Root, None, location) =>
      location.description shouldBe s"$Label: 5:10"
    }

  it should "report the tag written in the source" in {
    wrap(Id.Root, valueOf(config, "tagged-map")).tag shouldBe Some("!postgres")
    wrap(Id.Root, valueOf(config, "tagged-seq")).tag shouldBe Some("!identifiers")
    wrap(Id.Root, valueOf(config, "tagged-scalar")) should matchPattern {
      case Node.Scalar(Id.Root, Some("tag:yaml.org,2002:int"), "42", _) =>
    }
    wrap(Id.Root, valueOf(config, "tagged-nothing")) should matchPattern {
      case Node.Null(Id.Root, Some("tag:yaml.org,2002:null"), _) =>
    }
  }

  it should "reject a node kind that a composed document cannot contain" in {
    val anchor = new AnchorNode(valueOf(config, "scalar"))
    the[YamlEngineException] thrownBy wrap(Id.Root, anchor) should have message
      s"Unexpected ${anchor.getNodeType} node at $Label: 4:9"
  }

  "tagOf" should "return None for a tag the resolver inferred" in {
    tagOf(valueOf(config, "scalar")) shouldBe None
  }

  "keyOf" should "return the string value of the key node" in {
    keyOf(config.getValue.get(0)) shouldBe "map"
  }
}
