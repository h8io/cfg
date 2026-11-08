package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.exceptions.YamlEngineException

class ValidatedTest extends AnyFlatSpec with Matchers with Inside {
  "validated" should "reject a mapping that an alias makes contain itself" in {
    the[YamlEngineException] thrownBy parse("&a {self: *a}") should have message
      s"An alias makes the MAPPING at $Label: 1:1 contain itself"
  }

  it should "reject a sequence that an alias makes contain itself, however deep" in {
    the[YamlEngineException] thrownBy parse("outer:\n  inner: &s [1, {back: *s}]") should have message
      s"An alias makes the SEQUENCE at $Label: 2:10 contain itself"
  }

  it should "accept an anchor reused outside its own subtree" in {
    val cfg = wrap(Id.Root, yaml"base: &b {x: 1}\nfirst: *b\nsecond: [*b, *b]")
    inside(cfg("first")) { case first: Node.IMap[?] =>
      first("x") should matchPattern { case Node.Scalar(_, NoTags, "1", _) => }
    }
    inside(cfg("second")) { case second: Node.ISeq[?] => second.size shouldBe 2 }
  }

  "compose" should "reject a key that is not a scalar, which keyOf relies on" in {
    the[YamlEngineException] thrownBy parse("? [a, b]\n: x") should have message
      "Non scalar key is detected but it is not configured to be allowed."
  }
}
