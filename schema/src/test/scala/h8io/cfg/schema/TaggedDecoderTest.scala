package h8io.cfg.schema

import cats.syntax.all.*
import h8io.cfg.schema.errors.{AmbiguousMap, NonScalarTag, UnexpectedNode}
import h8io.cfg.schema.testutil.MockLocation
import h8io.cfg.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TaggedDecoderTest extends AnyFlatSpec with Matchers with MockFactory {
  private val loc = MockLocation("loc")

  "taggedDecoder" should "return the node as-is when it already has a tag" in {
    val map = mock[Node.Map]
    (() => map.tag).expects().returns(Some("existing"))
    Tagged.decoder(map) shouldBe Tagged("existing", map).valid
  }

  it should "use _ scalar as tag and strip it from the map" in {
    val map = mock[Node.Map]
    val stripped = mock[Node.Map]
    val scalar = Node.Scalar(Id.Key("_", Id.Root), None, "myTag", loc)
    (() => map.tag).expects().returns(None)
    (() => map.id).expects().returns(Id.Root)
    (map.apply(_: Id.Key)).expects(Id.Key("_", Id.Root)).returns(scalar)
    (map.-(_: String)).expects("_").returns(stripped)
    Tagged.decoder(map) shouldBe Tagged("myTag", stripped).valid
  }

  it should "return NonScalarTag when _ key is not a scalar" in {
    val map = mock[Node.Map]
    val nullNode = Node.Null(Id.Key("_", Id.Root), None, loc)
    (() => map.tag).expects().returns(None)
    (() => map.id).expects().returns(Id.Root)
    (map.apply(_: Id.Key)).expects(Id.Key("_", Id.Root)).returns(nullNode)
    Tagged.decoder(map) shouldBe NonScalarTag(map).invalid
  }

  it should "use the single key as tag when no _ key present" in {
    val map = mock[Node.Map]
    val value = Node.Scalar(Id.Key("kind", Id.Root), None, "circle", loc)
    (() => map.tag).expects().returns(None)
    (() => map.id).expects().returns(Id.Root)
    (map.apply(_: Id.Key)).expects(Id.Key("_", Id.Root)).returns(Node.None(Id.Key("_", Id.Root), map))
    (() => map.size).expects().returns(1)
    (() => map.iterator).expects().returns(Iterator(value))
    Tagged.decoder(map) shouldBe Tagged("kind", value).valid
  }

  it should "return AmbiguousMap when no _ key and multiple keys" in {
    val map = mock[Node.Map]
    (() => map.tag).expects().returns(None)
    (() => map.id).expects().returns(Id.Root)
    (map.apply(_: Id.Key)).expects(Id.Key("_", Id.Root)).returns(Node.None(Id.Key("_", Id.Root), map))
    (() => map.size).expects().returns(3)
    Tagged.decoder(map) shouldBe AmbiguousMap(map).invalid
  }

  it should "return AmbiguousMap for an empty map" in {
    val map = mock[Node.Map]
    (() => map.tag).expects().returns(None)
    (() => map.id).expects().returns(Id.Root)
    (map.apply(_: Id.Key)).expects(Id.Key("_", Id.Root)).returns(Node.None(Id.Key("_", Id.Root), map))
    (() => map.size).expects().returns(0)
    Tagged.decoder(map) shouldBe AmbiguousMap(map).invalid
  }

  it should "return UnexpectedNode for a scalar input" in {
    val scalar = Node.Scalar(Id.Root, None, "x", loc)
    Tagged.decoder(scalar) shouldBe UnexpectedNode[Tagged](scalar).invalid
  }
}
