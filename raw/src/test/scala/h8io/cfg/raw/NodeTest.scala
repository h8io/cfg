package h8io.cfg.raw

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NodeTest extends AnyFlatSpec with Matchers with MockFactory {
  "None" should "be created successfully" in {
    val key = Id.Key("unexistent", Id.Root)
    val parent = mock[Node.Map]
    Node.None(key, parent) shouldBe Node.INone(key, parent)
  }

  it should "be correctly extracted" in {
    val key = Id.Key("unexistent", Id.Root)
    val parent = mock[Node.Map]
    Node.INone(key, parent) should matchPattern { case Node.None(`key`, `parent`) => }
  }

  "Null" should "be created successfully" in {
    val key = Id.Index(42, Id.Root)
    val location = mock[Location]
    Node.Null(key, location) shouldBe Node.INull(key, location)
  }

  it should "be correctly extracted" in {
    val key = Id.Index(42, Id.Root)
    val location = mock[Location]
    Node.INull(key, location) should matchPattern { case Node.Null(`key`, `location`) => }
  }

  "Scalar" should "be created successfully" in {
    val key = Id.Key("great-one", Id.Root)
    val value = "Cthulhu"
    val location = mock[Location]
    val tag = Tag.None(location)
    Node.Scalar(key, value, tag, location) shouldBe Node.IScalar(key, value, tag, location)
  }

  it should "be correctly deconstructed" in {
    val key = Id.Key("great-one", Id.Root)
    val value = "Cthulhu"
    val location = mock[Location]
    val tag = Tag.Some("Great One", location)
    Node.IScalar(key, value, tag, location) should
      matchPattern { case Node.Scalar(`key`, `value`, `tag`, `location`) => }
  }

  "Map" should "be mocked successfully" in {
    "mock[Node.Map]" should compile
  }

  "Map.isEmpty" should "return true if size is 0" in {
    val map = mock[Node.IMap[Id]]
    (() => map.size).expects().returning(0)
    map.isEmpty shouldBe true
  }

  it should "return false if size is not 0" in {
    val map = mock[Node.IMap[Id]]
    (() => map.size).expects().returning(42)
    map.isEmpty shouldBe false
  }

  "Seq" should "be mocked successfully" in {
    "mock[Node.Seq]" should compile
  }

  "Seq.isEmpty" should "return true if size is 0" in {
    val seq = mock[Node.ISeq[Id]]
    (() => seq.size).expects().returning(0)
    seq.isEmpty shouldBe true
  }

  it should "return false if size is not 0" in {
    val seq = mock[Node.ISeq[Id]]
    (() => seq.size).expects().returning(42)
    seq.isEmpty shouldBe false
  }
}
