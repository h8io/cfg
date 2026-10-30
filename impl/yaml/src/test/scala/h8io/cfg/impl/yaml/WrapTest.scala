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
    inside(wrap(id, value)) { case map: Node.IMap[?] =>
      map should be(MapImpl(id, value.asInstanceOf[MappingNode]))
      map.tags shouldBe Set.empty
      map.iterator.map {
        inside(_) { case Node.Scalar(Id.Key(key, `id`), NoTags, scalar, _) => key -> scalar }
      }.toList should contain theSameElementsInOrderAs List("a" -> "x", "b" -> "y")
      map.location.description shouldBe s"$Label: 2:6"
    }
  }

  it should "create a Node.Seq object" in {
    val value = valueOf(config, "seq")
    inside(wrap(Id.Root, value)) { case seq: Node.ISeq[?] =>
      seq.iterator.zipWithIndex.map { case (node, i) =>
        inside(node) { case Node.Scalar(Id.Index(`i`, Id.Root), NoTags, scalar, _) => scalar }
      }.toList should contain theSameElementsInOrderAs List("1", "2", "3")
      seq.tags shouldBe Set.empty
      seq.location.description shouldBe s"$Label: 3:6"
    }
  }

  it should "create a Node.Scalar object" in
    inside(wrap(Id.Root, valueOf(config, "scalar"))) { case Node.Scalar(Id.Root, NoTags, "42", location) =>
      location.description shouldBe s"$Label: 4:9"
    }

  it should "create a Node.Null object" in
    inside(wrap(Id.Root, valueOf(config, "nothing"))) { case Node.Null(Id.Root, NoTags, location) =>
      location.description shouldBe s"$Label: 5:10"
    }

  it should "report the tag written in the source" in {
    wrap(Id.Root, valueOf(config, "tagged-map")).tags shouldBe Set("postgres")
    wrap(Id.Root, valueOf(config, "tagged-seq")).tags shouldBe Set("identifiers")
    wrap(Id.Root, valueOf(config, "tagged-scalar")) should matchPattern {
      case Node.Scalar(Id.Root, tags, "42", _) if tags == Set("tag:yaml.org,2002:int") =>
    }
    wrap(Id.Root, valueOf(config, "tagged-nothing")) should matchPattern {
      case Node.Null(Id.Root, tags, _) if tags == Set("tag:yaml.org,2002:null") =>
    }
  }

  it should "reject a node kind that a composed document cannot contain" in {
    val anchor = new AnchorNode(valueOf(config, "scalar"))
    the[YamlEngineException] thrownBy wrap(Id.Root, anchor) should have message
      s"Unexpected ${anchor.getNodeType.toString} node at $Label: 4:9"
  }

  "tagOf" should "return no tags for a tag the resolver inferred" in {
    tagOf(valueOf(config, "scalar")) shouldBe Set.empty
  }

  "keyOf" should "return the string value of the key node" in {
    keyOf(config.getValue.get(0)) shouldBe "map"
  }
}
