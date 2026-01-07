package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigOrigin, ConfigRenderOptions}
import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class SeqImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return Node.None if index is negative" in {
    val seq = SeqImpl(Id.Root, hocon"""list: [null, 42, 17]""".toConfig.getList("list"))
    inside(seq(-13)) { case Node.INone(Id.Index(-13, Id.Root), `seq`) => }
  }

  it should "return Node.None if index is equal to the array size" in {
    val list = hocon"""list: [1, 1, 2, 3, 5, 8]""".toConfig.getList("list")
    val seq = SeqImpl(Id.Root, list)
    val index = list.size
    seq(index) should matchPattern { case Node.INone(Id.Index(`index`, Id.Root), `seq`) => }
  }

  it should "return Node.None if index is greater than the array size" in {
    val list = hocon"""list: [one, two, three]""".toConfig.getList("list")
    val seq = SeqImpl(Id.Root, list)
    val index = list.size + 17
    seq(index) should matchPattern { case Node.INone(Id.Index(`index`, Id.Root), `seq`) => }
  }

  it should "return Node.Null" in {
    val list = hocon"""list: [three, two, one, null, one, two, three]""".toConfig.getList("list")
    inside(SeqImpl(Id.Root, list)(3)) { case Node.Null(Id.Index(3, Id.Root), LocationImpl(origin)) =>
      origin should be theSameInstanceAs list.get(3).origin
    }
  }

  it should "return Node.Scalar" in {
    val list = hocon"""list: [three, two, one]""".toConfig.getList("list")
    inside(SeqImpl(Id.Root, list)(0)) {
      case Node.Scalar(Id.Index(0, Id.Root), "three", LocationImpl(origin)) =>
        origin should be theSameInstanceAs list.get(0).origin
    }
  }

  it should "return Node.Seq" in {
    val list = hocon"""list: [three, [t, w, o], one]""".toConfig.getList("list")
    val nested = list.get(1).asInstanceOf[ConfigList]
    inside(SeqImpl(Id.Root, list)(1)) { case seq: Node.ISeq[Id.Index] =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = nested.get(i).origin
        val id = seq.id
        inside(value) {
          case Node.Scalar(Id.Index(`i`, `id`), scalar, LocationImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(scalar)
          case Node.Null(Id.Index(`i`, `id`), LocationImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs List(Some("t"), Some("w"), Some("o"))
      inside(seq.location) { case LocationImpl(origin) => origin should be theSameInstanceAs list.get(1).origin }
    }
  }

  it should "return Node.Map" in {
    val list = hocon"""list: [three, [t, w, o], {n: 2, o: 1, e: 3}]""".toConfig.getList("list")
    val index = list.size() - 1
    val obj = list.get(index).asInstanceOf[ConfigObject]
    inside(SeqImpl(Id.Root, list)(index)) { case map: Node.IMap[Id.Index] =>
      map.iterator.map { node =>
        inside(node) {
          case Node.Scalar(Id.Key(key, Id.Index(`index`, Id.Root)), scalar, LocationImpl(origin)) =>
            origin should be theSameInstanceAs obj.get(key).origin
            key -> Some(scalar)
          case Node.Null(Id.Key(key, Id.Index(`index`, Id.Root)), LocationImpl(origin)) =>
            origin should be theSameInstanceAs obj.get(key).origin
            key -> None
        }
      }.toList should contain theSameElementsAs List("o" -> Some("1"), "n" -> Some("2"), "e" -> Some("3"))
      inside(map.location) { case LocationImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of nodes" in {
    val list = hocon"""list: [a, null, b, c, null, "null"]""".toConfig.getList("list")
    SeqImpl(Id.Root, list).iterator.zipWithIndex.map { case (node, i) =>
      val expectedOrigin = list.get(i).origin
      inside(node) {
        case Node.Scalar(Id.Index(`i`, Id.Root), scalar, LocationImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          Some(scalar)
        case Node.Null(Id.Index(`i`, Id.Root), LocationImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          None
      }
    }.toList should contain theSameElementsAs List(Some("a"), None, Some("b"), Some("c"), None, Some("null"))
  }

  "size" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(25)
    SeqImpl(Id.Root, list).size shouldBe 25
  }

  "location" should "be a wrap on underlying origin object" in {
    val list = mock[ConfigList]
    val expectedOrigin = mock[ConfigOrigin]
    (list.origin _).expects().returns(expectedOrigin)
    inside(SeqImpl(Id.Root, list).location) { case LocationImpl(origin) =>
      origin should be theSameInstanceAs expectedOrigin
    }
  }

  "toString" should "delegate call to underlying.render" in {
    val list = mock[ConfigList]
    val expected = Random.nextString(16)
    (list.render(_: ConfigRenderOptions)).expects(RenderOptions).returns(expected)
    SeqImpl(Id.Root, list).toString should be theSameInstanceAs expected
  }
}
