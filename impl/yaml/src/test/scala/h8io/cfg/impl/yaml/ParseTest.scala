package h8io.cfg.impl.yaml

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.exceptions.YamlEngineException

import java.nio.charset.StandardCharsets

class ParseTest extends AnyFlatSpec with Matchers {
  private def parse(text: String): Node.ISome[Id.Root] = YAML.parse("x.yaml", text.getBytes(StandardCharsets.UTF_8))

  "YAML.parse" should "compose any node at the root, an empty document an empty mapping" in {
    parse("a: !t 1\n").toString shouldBe "a: !t '1'\n"
    parse("- a\n- b\n") match {
      case seq: Node.ISeq[?] => seq.size shouldBe 2
      case other => fail(other.toString)
    }
    parse("word") match {
      case Node.IScalar(Id.Root, None, "word", location) => location.description shouldBe "x.yaml: 1:1"
      case other => fail(other.toString)
    }
    parse("~") match {
      case Node.INull(Id.Root, None, _) => succeed
      case other => fail(other.toString)
    }
    parse("") match {
      case map: Node.IMap[?] => map.size shouldBe 0
      case other => fail(other.toString)
    }
  }

  it should "read any encoding YAML allows" in {
    val utf16 = "﻿a: b\n".getBytes(StandardCharsets.UTF_16BE)
    YAML.parse("x.yaml", utf16).toString shouldBe "a: b\n"
  }

  it should "refuse an alias to a mapping or a sequence not yet ended, a merge key's too, which would never end" in {
    the[YamlEngineException] thrownBy parse("a: &a {<<: *a}") should have message
      "An alias makes the MAPPING at x.yaml: 1:4 contain itself"
    // an anchor defined again names the node defined last
    def sizes(text: String): List[Int] =
      parse(text) match {
        case root: Node.IMap[?] =>
          root("a") match {
            case seq: Node.ISeq[?] =>
              seq.iterator.map {
                case inner: Node.ISeq[?] => inner.size
                case _ => 0
              }.toList
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    sizes("a: &a [&a x, *a]") shouldBe List(0, 0)
    sizes("a: &a [&a [1], *a]") shouldBe List(1, 1)
  }

  it should "refuse what the loader refuses" in {
    the[YamlEngineException] thrownBy parse("a: &x [*x]") should have message
      "An alias makes the SEQUENCE at x.yaml: 1:4 contain itself"
    an[YamlEngineException] should be thrownBy parse("a: 1\n---\nb: 2\n")
  }
}
