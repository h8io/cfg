package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeqImplTest extends AnyFlatSpec with Matchers with Inside {
  "apply" should "return Node.None if index is negative" in {
    val seq = SeqImpl(Id.Root, list"[~, 42, 17]")
    seq(-13) should matchPattern { case Node.INone(Id.Index(-13, Id.Root), `seq`) => }
  }

  it should "return Node.None if index is equal to the sequence size" in {
    val seq = SeqImpl(Id.Root, list"[1, 1, 2, 3, 5, 8]")
    val index = seq.size
    seq(index) should matchPattern { case Node.INone(Id.Index(`index`, Id.Root), `seq`) => }
  }

  it should "return Node.None if index is greater than the sequence size" in {
    val seq = SeqImpl(Id.Root, list"[one, two, three]")
    val index = seq.size + 17
    seq(index) should matchPattern { case Node.INone(Id.Index(`index`, Id.Root), `seq`) => }
  }

  it should "return Node.Null" in {
    SeqImpl(Id.Root, list"[three, two, one, ~]")(3) should matchPattern {
      case Node.Null(Id.Index(3, Id.Root), None, _) =>
    }
  }

  it should "return Node.Scalar" in
    inside(SeqImpl(Id.Root, list"[three, two, one]")(0)) {
      case Node.Scalar(Id.Index(0, Id.Root), None, "three", location) =>
        location.description shouldBe s"$Label: 1:2"
    }

  it should "return Node.Seq" in
    inside(SeqImpl(Id.Root, list"[three, [t, w, o]]")(1)) { case seq: Node.ISeq[Id.Index] =>
      val id = seq.id
      seq.iterator.map {
        inside(_) { case Node.Scalar(Id.Index(_, `id`), None, scalar, _) => scalar }
      }.toList should contain theSameElementsInOrderAs List("t", "w", "o")
    }

  it should "return Node.Map" in
    inside(SeqImpl(Id.Root, list"[three, {n: 2, o: 1}]")(1)) { case map: Node.IMap[Id.Index] =>
      map.iterator.map {
        inside(_) { case Node.Scalar(Id.Key(key, Id.Index(1, Id.Root)), None, scalar, _) => key -> scalar }
      }.toList should contain theSameElementsInOrderAs List("n" -> "2", "o" -> "1")
    }

  "tag" should "return None when the source carries no tag" in {
    SeqImpl(Id.Root, list"[1, 2]").tag shouldBe None
  }

  it should "return the tag written in the source" in {
    SeqImpl(Id.Root, list"!identifiers [1, 2]").tag shouldBe Some("!identifiers")
  }

  "iterator" should "return a correct sequence of nodes" in {
    SeqImpl(Id.Root, list"""[a, ~, b, c, ~, "null"]""").iterator.zipWithIndex.map { case (node, i) =>
      inside(node) {
        case Node.Scalar(Id.Index(`i`, Id.Root), None, scalar, _) => Some(scalar)
        case Node.Null(Id.Index(`i`, Id.Root), None, _) => None
      }
    }.toList should contain theSameElementsInOrderAs List(Some("a"), None, Some("b"), Some("c"), None, Some("null"))
  }

  "size" should "return the number of the elements" in {
    SeqImpl(Id.Root, list"[a, b, c, d]").size shouldBe 4
  }

  "location" should "be the position of the sequence in the source" in {
    val sequence = list"""
- a
- b
"""
    SeqImpl(Id.Root, sequence).location.description shouldBe s"$Label: 2:1"
  }

  "toString" should "render the sequence back to YAML" in {
    SeqImpl(Id.Root, list"[a, [b, c]]").toString shouldBe "[a, [b, c]]\n"
  }
}
