package h8io.cfg.raw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IdTest extends AnyFlatSpec with Matchers {
  "Root.description" should "return the \"root\" string" in {
    Id.Root.path shouldBe empty
  }

  "Key.path" should "return a correct path if the parent is Id.Root" in {
    Id.Key("property-name", Id.Root).path shouldBe "property-name"
  }

  it should "return a correct path if the parent is Id.Key" in {
    Id.Key("property-key", Id.Key("parent-key", Id.Root)).path shouldBe "parent-key.property-key"
  }

  it should "return a correct path if the parent is Id.Index" in {
    Id.Key("property-key", Id.Index(12, Id.Root)).path shouldBe "[12].property-key"
  }

  it should "quote keys with periods" in {
    Id.Key("property.key", Id.Root).path shouldBe "\"property.key\""
    Id.Key("property.key", Id.Index(1024, Id.Root)).path shouldBe "[1024].\"property.key\""
    Id.Key("property.key", Id.Key("parent.key", Id.Root)).path shouldBe "\"parent.key\".\"property.key\""
    Id.Key("property.", Id.Key(".key", Id.Root)).path shouldBe "\".key\".\"property.\""
    Id.Key(".key", Id.Key("parent.", Id.Root)).path shouldBe "\"parent.\".\".key\""
    Id.Key("property-key", Id.Key("parent.key", Id.Root)).path shouldBe "\"parent.key\".property-key"
    Id.Key("property-key", Id.Key("-key", Id.Root)).path shouldBe "\"-key\".property-key"
    Id.Key("-key", Id.Key("parent-key", Id.Root)).path shouldBe "parent-key.\"-key\""
    Id.Key("property.key", Id.Key("parent-key", Id.Root)).path shouldBe "parent-key.\"property.key\""
    Id.Index(256, Id.Key("parent.key", Id.Root)).path shouldBe "\"parent.key\"[256]"
  }

  it should "quote keys with unsafe characters" in {
    val unsafe = "\"\\\b\f\n\r\t Spaces: \u00A0\u3000; Format: \u200C\u2069\uFEFF;" +
      " Controls: \u0000\u001B\u0080\u0084\u009C; Separators: \u2028\u2029;" +
      " High surrogates: \uD800\uD8A9\uDB7F; Low surrogates \uDC00\uDD42\uDFF1."
    val escaped = "\\\"\\\\\\b\\f\\n\\r\\t Spaces: \\u00A0\\u3000; Format: \\u200C\\u2069\\uFEFF;" +
      " Controls: \\u0000\\u001B\\u0080\\u0084\\u009C; Separators: \\u2028\\u2029;" +
      " High surrogates: \\uD800\\uD8A9\\uDB7F; Low surrogates \\uDC00\\uDD42\\uDFF1."
    Id.Key("property-" + unsafe, Id.Root).path shouldBe "\"property-" + escaped + '"'
    Id.Key("property-" + unsafe, Id.Index(1024, Id.Root)).path shouldBe "[1024].\"property-" + escaped + '"'
    Id.Key("property-" + unsafe, Id.Key("parent-" + unsafe, Id.Root)).path shouldBe
      "\"parent-" + escaped + "\".\"property-" + escaped + '"'
    Id.Key("property-key", Id.Key("parent-" + unsafe, Id.Root)).path shouldBe "\"parent-" + escaped + "\".property-key"
    Id.Key("property-" + unsafe, Id.Key("parent-key", Id.Root)).path shouldBe "parent-key.\"property-" + escaped + '"'
    Id.Index(256, Id.Key("parent-" + unsafe, Id.Root)).path shouldBe "\"parent-" + escaped + "\"[256]"
  }

  it should "quote keys with emojis" in {
    val unsafe = "😀💀🐯🌈💋🍆👠"
    val escaped = "\\uD83D\\uDE00\\uD83D\\uDC80\\uD83D\\uDC2F\\uD83C\\uDF08\\uD83D\\uDC8B\\uD83C\\uDF46\\uD83D\\uDC60"
    Id.Key("property-" + unsafe, Id.Root).path shouldBe "\"property-" + escaped + '"'
    Id.Key("property-" + unsafe, Id.Index(1024, Id.Root)).path shouldBe "[1024].\"property-" + escaped + '"'
    Id.Key("property-" + unsafe, Id.Key("parent-" + unsafe, Id.Root)).path shouldBe
      "\"parent-" + escaped + "\".\"property-" + escaped + '"'
    Id.Key("property-key", Id.Key("parent-" + unsafe, Id.Root)).path shouldBe "\"parent-" + escaped + "\".property-key"
    Id.Key("property-" + unsafe, Id.Key("parent-key", Id.Root)).path shouldBe "parent-key.\"property-" + escaped + '"'
    Id.Index(256, Id.Key("parent-" + unsafe, Id.Root)).path shouldBe "\"parent-" + escaped + "\"[256]"
  }

  it should "quote empty keys" in {
    Id.Key("", Id.Root).path shouldBe "\"\""
    Id.Key("", Id.Index(1024, Id.Root)).path shouldBe "[1024].\"\""
    Id.Key("", Id.Key("", Id.Root)).path shouldBe "\"\".\"\""
    Id.Key("property-key", Id.Key("", Id.Root)).path shouldBe "\"\".property-key"
    Id.Key("", Id.Key("parent-key", Id.Root)).path shouldBe "parent-key.\"\""
    Id.Index(256, Id.Key("", Id.Root)).path shouldBe "\"\"[256]"
  }

  it should "not quote unicode letters" in {
    Id.Key("кошмар", Id.Root).path shouldBe "кошмар"
    Id.Key("怪談", Id.Index(4, Id.Root)).path shouldBe "[4].怪談"
    Id.Index(13, Id.Key("მოჩვენება", Id.Root)).path shouldBe "მოჩვენება[13]"
    Id.Key("भय", Id.Key("피", Id.Root)).path shouldBe "피.भय"
  }

  "Index.path" should "return a correct path if the parent is Id.Root" in {
    Id.Index(42, Id.Root).path shouldBe "[42]"
  }

  it should "return a correct path if the parent is Id.Key" in {
    Id.Index(3, Id.Key("parent-key", Id.Root)).path shouldBe "parent-key[3]"
  }

  it should "return a correct path if the parent is Id.Index" in {
    Id.Index(17, Id.Index(0, Id.Root)).path shouldBe "[0][17]"
  }

  "Index.fits" should "check if the index is in bounds of collection with size n" in {
    Id.Index(0, Id.Root).fits(1) shouldBe true
    Id.Index(16, Id.Root).fits(17) shouldBe true
    Id.Index(24, Id.Root).fits(42) shouldBe true

    Id.Index(0, Id.Root).fits(0) shouldBe false
    Id.Index(-1, Id.Root).fits(1) shouldBe false
    Id.Index(1, Id.Root).fits(1) shouldBe false
    Id.Index(42, Id.Root).fits(17) shouldBe false
  }
}
