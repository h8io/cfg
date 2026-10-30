package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TagOfTest extends AnyFlatSpec with Matchers {
  "tagOf" should "return the name of the tag written on a scalar, a sequence and a mapping" in {
    val config = yaml"""
scalar: !!int 42
sequence: !identifiers [1]
mapping: !postgres {host: localhost}
"""
    tagOf(config) shouldBe Set.empty
    tagOf(valueOf(config, "scalar")) shouldBe Set("tag:yaml.org,2002:int")
    tagOf(valueOf(config, "sequence")) shouldBe Set("identifiers")
    tagOf(valueOf(config, "mapping")) shouldBe Set("postgres")
  }

  it should "return no tags when the resolver inferred the tag" in {
    val config = yaml"""
scalar: 42
sequence: [1]
mapping: {host: localhost}
nothing: ~
"""
    tagOf(valueOf(config, "scalar")) shouldBe Set.empty
    tagOf(valueOf(config, "sequence")) shouldBe Set.empty
    tagOf(valueOf(config, "mapping")) shouldBe Set.empty
    tagOf(valueOf(config, "nothing")) shouldBe Set.empty
  }

  it should "return no tags for the non-specific tag" in {
    val config = yaml"""
scalar: ! "42"
sequence: ! [1]
mapping: ! {host: localhost}
"""
    tagOf(valueOf(config, "scalar")) shouldBe Set.empty
    tagOf(valueOf(config, "sequence")) shouldBe Set.empty
    tagOf(valueOf(config, "mapping")) shouldBe Set.empty
  }

  it should "drop the leading ! of a local tag, so that it gives the same name as a verbatim one" in {
    val config = yaml"""
local: !postgres {host: localhost}
verbatim: !<postgres> {host: localhost}
"""
    tagOf(valueOf(config, "local")) shouldBe Set("postgres")
    tagOf(valueOf(config, "verbatim")) shouldBe Set("postgres")
  }

  it should "return the tag of the document root" in {
    tagOf(yaml"!postgres {host: localhost}") shouldBe Set("postgres")
  }

  it should "return the tag of the anchor for an alias" in {
    val config = yaml"""
anchored: !postgres &db {host: localhost}
alias: *db
"""
    valueOf(config, "alias") should be theSameInstanceAs valueOf(config, "anchored")
    tagOf(valueOf(config, "alias")) shouldBe Set("postgres")
  }
}
