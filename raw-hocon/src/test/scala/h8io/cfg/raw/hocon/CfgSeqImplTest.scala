package h8io.cfg.raw.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigOrigin}
import context.CfgContext
import h8io.cfg.raw.{Entry, Ref}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CfgSeqImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return CfgNone if index is negative" in {
    val list = hocon"""list: [null, 42, 17]""".toConfig.getList("list")
    inside(SeqImpl(Ref.Root, list)(-13)) { case Entry.None(Ref.Index(-13), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return CfgNone if index is equal to the array size" in {
    val list = hocon"""list: [1, 1, 2, 3, 5, 8]""".toConfig.getList("list")
    val index = list.size + 17
    inside(SeqImpl(Ref.Root, list)(index)) { case Entry.None(Ref.Index(`index`), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return CfgNone if index is greater than the array size" in {
    val list = hocon"""list: [one, two, three]""".toConfig.getList("list")
    val index = list.size + 17
    inside(SeqImpl(Ref.Root, list)(index)) { case Entry.None(Ref.Index(`index`), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return CfgNull" in {
    val list = hocon"""list: [three, two, one, null, one, two, three]""".toConfig.getList("list")
    inside(SeqImpl(Ref.Root, list)(3)) { case Entry.Null(Ref.Index(3), OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.get(3).origin
    }
  }

  it should "return CfgScalar" in {
    val list = hocon"""list: [three, two, one]""".toConfig.getList("list")
    inside(SeqImpl(Ref.Root, list)(0)) { case Entry.Scalar(Ref.Index(0), "three", OriginImpl(origin)) =>
      origin should be theSameInstanceAs list.get(0).origin
    }
  }

  it should "return CfgSeq" in {
    val list = hocon"""list: [three, [t, w, o], one]""".toConfig.getList("list")
    val nested = list.get(1).asInstanceOf[ConfigList]
    inside(SeqImpl(Ref.Root, list)(1)) { case seq: Entry.Seq[?] =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = nested.get(i).origin
        inside(value) {
          case Entry.Scalar(Ref.Index(`i`), value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case Entry.Null(Ref.Index(`i`), OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs List(Some("t"), Some("w"), Some("o"))
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs list.get(1).origin }
    }
  }

  it should "return CfgMap" in {
    val list = hocon"""list: [three, [t, w, o], {n: 2, o: 1, e: 3}]""".toConfig.getList("list")
    val index = list.size() - 1
    val obj = list.get(index).asInstanceOf[ConfigObject]
    inside(SeqImpl(Ref.Root, list)(index)) { case seq: Entry.Map[?] =>
      seq.iterator.map { entry =>
        val expectedOrigin = obj.get(entry.id.key).origin
        inside(entry) {
          case Entry.Scalar(path, value, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            path.key -> Some(value)
          case Entry.Null(path, OriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            path.key -> None
        }
      }.toList should contain theSameElementsAs List("o" -> Some("1"), "n" -> Some("2"), "e" -> Some("3"))
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of entries" in {
    val list = hocon"""list: [a, null, b, c, null, "null"]""".toConfig.getList("list")
    SeqImpl(Ref.Root, list).iterator.zipWithIndex.map { case (entry, i) =>
      val expectedOrigin = list.get(i).origin
      inside(entry) {
        case Entry.Scalar(Ref.Index(`i`), value, OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          Some(value)
        case Entry.Null(Ref.Index(`i`), OriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          None
      }
    }.toList should contain theSameElementsAs List(Some("a"), None, Some("b"), Some("c"), None, Some("null"))
  }

  "isEmpty" should "return true" in {
    val list = mock[ConfigList]
    (list.isEmpty _).expects().returns(true)
    SeqImpl(Ref.Root, list).isEmpty shouldBe true
  }

  it should "return false" in {
    val list = mock[ConfigList]
    (list.isEmpty _).expects().returns(false)
    SeqImpl(Ref.Root, list).isEmpty shouldBe false
  }

  "size" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(25)
    SeqImpl(Ref.Root, list).size shouldBe 25
  }

  "knownSize" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(1024)
    SeqImpl(Ref.Root, list).knownSize shouldBe 1024
  }

  "origin" should "be a wrap on underlying origin object" in {
    val list = mock[ConfigList]
    val expectedOrigin = mock[ConfigOrigin]
    (list.origin _).expects().returns(expectedOrigin)
    inside(SeqImpl(Ref.Root, list).origin) { case OriginImpl(origin) =>
      origin should be theSameInstanceAs expectedOrigin
    }
  }
}
