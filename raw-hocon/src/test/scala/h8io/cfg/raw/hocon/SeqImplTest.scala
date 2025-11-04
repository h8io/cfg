package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigOrigin}
import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeqImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return Node.None if index is negative" in {
    val list = hocon"""list: [null, 42, 17]""".toConfig.getList("list")
    inside(SeqImpl(Id.Root, list)(-13)) { case Node.None(Id.Index(-13), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return Node.None if index is equal to the array size" in {
    val list = hocon"""list: [1, 1, 2, 3, 5, 8]""".toConfig.getList("list")
    val index = list.size + 17
    inside(SeqImpl(Id.Root, list)(index)) { case Node.None(Id.Index(`index`), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return Node.None if index is greater than the array size" in {
    val list = hocon"""list: [one, two, three]""".toConfig.getList("list")
    val index = list.size + 17
    inside(SeqImpl(Id.Root, list)(index)) { case Node.None(Id.Index(`index`), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return Node.Null" in {
    val list = hocon"""list: [three, two, one, null, one, two, three]""".toConfig.getList("list")
    inside(SeqImpl(Id.Root, list)(3)) { case Node.Null(Id.Index(3), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.get(3).origin
    }
  }

  it should "return Node.Scalar" in {
    val list = hocon"""list: [three, two, one]""".toConfig.getList("list")
    inside(SeqImpl(Id.Root, list)(0)) { case Node.Scalar(Id.Index(0), "three", OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.get(0).origin
    }
  }

  it should "return Node.Seq" in {
    val list = hocon"""list: [three, [t, w, o], one]""".toConfig.getList("list")
    val nested = list.get(1).asInstanceOf[ConfigList]
    inside(SeqImpl(Id.Root, list)(1)) { case seq: Node.Seq[?] =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = nested.get(i).origin
        inside(value) {
          case Node.Scalar(Id.Index(`i`), value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case Node.Null(Id.Index(`i`), OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs List(Some("t"), Some("w"), Some("o"))
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs list.get(1).origin }
    }
  }

  it should "return Node.Map" in {
    val list = hocon"""list: [three, [t, w, o], {n: 2, o: 1, e: 3}]""".toConfig.getList("list")
    val index = list.size() - 1
    val obj = list.get(index).asInstanceOf[ConfigObject]
    inside(SeqImpl(Id.Root, list)(index)) { case seq: Node.Map[?] =>
      seq.iterator.map { node =>
        val expectedOrigin = obj.get(node.id.key).origin
        inside(node) {
          case Node.Scalar(id, value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            id.key -> Some(value)
          case Node.Null(id, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            id.key -> None
        }
      }.toList should contain theSameElementsAs List("o" -> Some("1"), "n" -> Some("2"), "e" -> Some("3"))
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of nodes" in {
    val list = hocon"""list: [a, null, b, c, null, "null"]""".toConfig.getList("list")
    SeqImpl(Id.Root, list).iterator.zipWithIndex.map { case (node, i) =>
      val expectedOrigin = list.get(i).origin
      inside(node) {
        case Node.Scalar(Id.Index(`i`), value, OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          Some(value)
        case Node.Null(Id.Index(`i`), OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          None
      }
    }.toList should contain theSameElementsAs List(Some("a"), None, Some("b"), Some("c"), None, Some("null"))
  }

  "isEmpty" should "return true" in {
    val list = mock[ConfigList]
    (list.isEmpty _).expects().returns(true)
    SeqImpl(Id.Root, list).isEmpty shouldBe true
  }

  it should "return false" in {
    val list = mock[ConfigList]
    (list.isEmpty _).expects().returns(false)
    SeqImpl(Id.Root, list).isEmpty shouldBe false
  }

  "size" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(25)
    SeqImpl(Id.Root, list).size shouldBe 25
  }

  "knownSize" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(1024)
    SeqImpl(Id.Root, list).knownSize shouldBe 1024
  }

  "origin" should "be a wrap on underlying origin object" in {
    val list = mock[ConfigList]
    val expectedOrigin = mock[ConfigOrigin]
    (list.origin _).expects().returns(expectedOrigin)
    inside(SeqImpl(Id.Root, list).origin) { case OriginImpl(origin) =>
      origin should be theSameInstanceAs expectedOrigin
    }
  }
}
