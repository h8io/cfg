package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TaggedComposerTest extends AnyFlatSpec with Matchers {
  "TaggedComposer" should "record the tag written on a scalar, a sequence and a mapping" in {
    val config = yaml"""
scalar: !!int 42
sequence: !identifiers [1]
mapping: !postgres {host: localhost}
"""
    tagOf(config) shouldBe None
    tagOf(valueOf(config, "scalar")) shouldBe Some("tag:yaml.org,2002:int")
    tagOf(valueOf(config, "sequence")) shouldBe Some("!identifiers")
    tagOf(valueOf(config, "mapping")) shouldBe Some("!postgres")
  }

  it should "record nothing when the resolver inferred the tag" in {
    val config = yaml"""
scalar: 42
sequence: [1]
mapping: {host: localhost}
nothing: ~
"""
    tagOf(valueOf(config, "scalar")) shouldBe None
    tagOf(valueOf(config, "sequence")) shouldBe None
    tagOf(valueOf(config, "mapping")) shouldBe None
    tagOf(valueOf(config, "nothing")) shouldBe None
  }

  it should "record nothing for the non-specific tag" in {
    val config = yaml"""
scalar: ! "42"
sequence: ! [1]
mapping: ! {host: localhost}
"""
    tagOf(valueOf(config, "scalar")) shouldBe None
    tagOf(valueOf(config, "sequence")) shouldBe None
    tagOf(valueOf(config, "mapping")) shouldBe None
  }

  it should "record the tag of the document root" in {
    tagOf(yaml"!postgres {host: localhost}") shouldBe Some("!postgres")
  }

  it should "carry the tag over to an alias of a tagged node" in {
    val config = yaml"""
anchored: !postgres &db {host: localhost}
alias: *db
"""
    valueOf(config, "alias") should be theSameInstanceAs valueOf(config, "anchored")
    tagOf(valueOf(config, "alias")) shouldBe Some("!postgres")
  }
}
