package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.nodes.ScalarNode

class MergeTest extends AnyFlatSpec with Matchers with Inside {
  private def merged(previous: String, next: String): Node.ISome[Id.Root] =
    wrap(Id.Root, merge(parse(previous), parse(next)))

  private def scalars(node: Node.ISome[?]): List[(String, String)] =
    inside(node) { case map: Node.IMap[?] =>
      map.iterator.map {
        inside(_) { case Node.Scalar(Id.Key(key, _), _, scalar, _) => key -> scalar }
      }.toList
    }

  "merge" should "keep the entries that the overlay does not mention" in {
    scalars(merged("{a: 1, b: 2}", "{b: 3}")) should contain theSameElementsInOrderAs List("a" -> "1", "b" -> "3")
  }

  it should "append the entries that only the overlay defines" in {
    scalars(merged("{a: 1}", "{b: 2, c: 3}")) should
      contain theSameElementsInOrderAs List("a" -> "1", "b" -> "2", "c" -> "3")
  }

  it should "merge nested mappings key by key" in
    inside(merged("{server: {host: h, port: 1}}", "{server: {port: 2, secure: true}}")) {
      case map: Node.IMap[Id.Root] =>
        scalars(map("server").asInstanceOf[Node.ISome[Id.Key]]) should
          contain theSameElementsInOrderAs List("host" -> "h", "port" -> "2", "secure" -> "true")
    }

  it should "replace a sequence rather than concatenate it" in
    inside(merged("{hosts: [a, b, c]}", "{hosts: [x]}")) { case map: Node.IMap[Id.Root] =>
      inside(map("hosts")) { case hosts: Node.ISeq[Id.Key] =>
        hosts.iterator.map {
          inside(_) { case Node.Scalar(_, _, scalar, _) => scalar }
        }.toList should contain theSameElementsInOrderAs List("x")
      }
    }

  it should "replace a mapping by a scalar" in {
    scalars(merged("{a: {b: 1}}", "{a: 2}")) should contain theSameElementsInOrderAs List("a" -> "2")
  }

  it should "replace a scalar by a mapping" in
    inside(merged("{a: 1}", "{a: {b: 2}}")) { case map: Node.IMap[Id.Root] =>
      scalars(map("a").asInstanceOf[Node.ISome[Id.Key]]) should contain theSameElementsInOrderAs List("b" -> "2")
    }

  it should "return the overlay when neither side is a mapping" in {
    val previous = parse("[a]")
    val overlay = parse("[b]")
    merge(previous, overlay) should be theSameInstanceAs overlay
  }

  it should "keep the tag and the location of the overlay" in {
    inside(merged("{a: 1}", "!postgres {a: 2}")) { case map: Node.IMap[Id.Root] =>
      map.tag shouldBe Some("!postgres")
      map.location.description shouldBe s"$Label: 1:1"
    }
    merged("{a: 1}", "{a: 2}").tag shouldBe None
  }

  "entriesOf" should "collapse the entries sharing a key onto the last one" in {
    entriesOf(yaml"{a: 1, b: 2, a: 3}").map { case (key, tuple) =>
      key -> tuple.getValueNode.asInstanceOf[ScalarNode].getValue
    } should contain theSameElementsInOrderAs List("b" -> "2", "a" -> "3")
  }
}
