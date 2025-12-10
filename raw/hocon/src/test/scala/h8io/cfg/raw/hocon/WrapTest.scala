package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigFactory, ConfigObject, ConfigValue}
import h8io.cfg.raw.{Id, Node, Tag}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WrapTest extends AnyFlatSpec with Matchers with Inside {
  private val config = ConfigFactory.load("wrap-test.conf").root()

  "wrap" should "create a Node.Map object" in testMap("map", v => Tag.None(LocationImpl(v)))

  it should "create a Node.Map object with null tag" in testMap("map-with-null-tag", v => Tag.None(LocationImpl(v)))

  it should "create a Node.Map object with empty tag" in testMap("map-with-empty-tag", v => Tag.None(LocationImpl(v)))

  it should "create a Node.Map object with scalar tag" in
    testMap("map-with-scalar-tag", v => Tag.Some("map-tag", LocationImpl(v)))

  it should "create a Node.Map object with map tag" in testMap("map-with-map-tag", UnsupportedTag(_))

  it should "create a Node.Map object with seq tag" in testMap("map-with-seq-tag", UnsupportedTag(_))

  def testMap(dataKey: String, tag: ConfigValue => Tag): Unit = {
    val mapValue = config.get(dataKey)
    val expectedTag = tag(Option(mapValue.asInstanceOf[ConfigObject].get(TagKey)).getOrElse(mapValue))
    val rootId = Id.Index(42, Id.Root)
    inside(wrap(rootId, mapValue)) { case map: Node.IMap[Id.Index] =>
      map.tag shouldBe expectedTag
      map.iterator.map {
        inside(_) {
          case Node.Scalar(Id.Key(key, `rootId`), scalar, Tag.None(tagLocation), location) =>
            tagLocation shouldBe location
            key -> Some(scalar)
          case Node.Null(Id.Key(key, `rootId`), _) => key -> None
        }
      }.toList should contain theSameElementsAs
        List("a" -> Some("x"), "b" -> Some("y"), "c" -> Some("z"), "d" -> None)
      inside(map.location) { case LocationImpl(origin) => origin should be theSameInstanceAs mapValue.origin }
    }
  }

  it should "create a Node.Seq object" in {
    val seqValue = config.get("seq")
    inside(wrap(Id.Root, seqValue)) { case seq: Node.ISeq[Id.Root] =>
      seq.tag shouldBe Tag.None(LocationImpl(seqValue))
      seq.iterator.zipWithIndex.map { case (node, i) =>
        val id = seq.id
        inside(node) {
          case Node.Scalar(Id.Index(`i`, `id`), scalar, Tag.None(tagLocation), location) =>
            tagLocation shouldBe location
            Some(scalar)
          case Node.Null(Id.Index(`i`, `id`), _) => None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("1"), Some("2"), Some("3"), None)
      inside(seq.location) { case LocationImpl(origin) => origin should be theSameInstanceAs seqValue.origin }
    }
  }

  it should "create a Node.Scalar object" in {
    val scalarValue = config.get("scalar")
    inside(wrap(Id.Root, scalarValue)) {
      case Node.Scalar(Id.Root, "42", Tag.None(LocationImpl(tagOrigin)), LocationImpl(origin)) =>
        origin should be theSameInstanceAs scalarValue.origin
        tagOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a Node.Scalar object with empty tag" in {
    val scalarValue = config.get("scalar-with-empty-tag")
    inside(wrap(Id.Root, scalarValue)) {
      case Node.Scalar(Id.Root, "Cthulhu::The Great", Tag.None(LocationImpl(tagOrigin)), LocationImpl(origin)) =>
        origin should be theSameInstanceAs scalarValue.origin
        tagOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a Node.Scalar object with tag" in {
    val scalarValue = config.get("scalar-with-tag")
    inside(wrap(Id.Root, scalarValue)) {
      case Node.Scalar(Id.Root, "42", Tag.Some("int", LocationImpl(tagOrigin)), LocationImpl(origin)) =>
        origin should be theSameInstanceAs scalarValue.origin
        tagOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a Node.Null object" in {
    val nullValue = config.get("null")
    inside(wrap(Id.Root, nullValue)) { case Node.Null(Id.Root, LocationImpl(origin)) =>
      origin should be theSameInstanceAs nullValue.origin
    }
  }
}
