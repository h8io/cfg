package h8io.cfg.impl.hocon

import com.typesafe.config.{ConfigList, ConfigObject, ConfigOrigin}
import h8io.cfg.impl.hocon.context.CfgContext
import h8io.cfg.{CfgMap, CfgNone, CfgNull, CfgScalar, CfgSeq}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CfgSeqImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return CfgNone if index is negative" in {
    val list = hocon"""list: [null, 42, 17]""".toConfig.getList("list")
    inside(CfgSeqImpl(list)(-13)) { case CfgNone(CfgOriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return CfgNone if index is equal to the array size" in {
    val list = hocon"""list: [1, 1, 2, 3, 5, 8]""".toConfig.getList("list")
    inside(CfgSeqImpl(list)(list.size)) { case CfgNone(CfgOriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return CfgNone if index is greater than the array size" in {
    val list = hocon"""list: [one, two, three]""".toConfig.getList("list")
    inside(CfgSeqImpl(list)(list.size + 17)) { case CfgNone(CfgOriginImpl(origin)) =>
      origin should be theSameInstanceAs list.origin
    }
  }

  it should "return CfgNull" in {
    val list = hocon"""list: [three, two, one, null, one, two, three]""".toConfig.getList("list")
    inside(CfgSeqImpl(list)(3)) { case CfgNull(CfgOriginImpl(origin)) =>
      origin should be theSameInstanceAs list.get(3).origin
    }
  }

  it should "return CfgScalar" in {
    val list = hocon"""list: [three, two, one]""".toConfig.getList("list")
    inside(CfgSeqImpl(list)(0)) { case CfgScalar("three", CfgOriginImpl(origin)) =>
      origin should be theSameInstanceAs list.get(0).origin
    }
  }

  it should "return CfgSeq" in {
    val list = hocon"""list: [three, [t, w, o], one]""".toConfig.getList("list")
    val nested = list.get(1).asInstanceOf[ConfigList]
    inside(CfgSeqImpl(list)(1)) { case seq: CfgSeq =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = nested.get(i).origin
        inside(value) {
          case CfgScalar(value, CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case CfgNull(CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs List(Some("t"), Some("w"), Some("o"))
      inside(seq.origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs list.get(1).origin }
    }
  }

  it should "return CfgMap" in {
    val list = hocon"""list: [three, [t, w, o], {n: 2, o: 1, e: 3}]""".toConfig.getList("list")
    val index = list.size() - 1
    val obj = list.get(index).asInstanceOf[ConfigObject]
    inside(CfgSeqImpl(list)(index)) { case seq: CfgMap =>
      seq.iterator.map { case (key, value) =>
        val expectedOrigin = obj.get(key).origin
        inside(value) {
          case CfgScalar(value, CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            key -> Some(value)
          case CfgNull(CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            key -> None
        }
      }.toList should contain theSameElementsAs List("o" -> Some("1"), "n" -> Some("2"), "e" -> Some("3"))
      inside(seq.origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }

  "iterator" should "return a correct sequence of entries" in {
    val list = hocon"""list: [a, null, b, c, null, "null"]""".toConfig.getList("list")
    CfgSeqImpl(list).iterator.zipWithIndex.map { case (value, i) =>
      val expectedOrigin = list.get(i).origin
      inside(value) {
        case CfgScalar(value, CfgOriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          Some(value)
        case CfgNull(CfgOriginImpl(origin)) =>
          origin should be theSameInstanceAs expectedOrigin
          None
      }
    }.toList should contain theSameElementsAs List(Some("a"), None, Some("b"), Some("c"), None, Some("null"))
  }

  "isEmpty" should "return true" in {
    val list = mock[ConfigList]
    (list.isEmpty _).expects().returns(true)
    CfgSeqImpl(list).isEmpty shouldBe true
  }

  it should "return false" in {
    val list = mock[ConfigList]
    (list.isEmpty _).expects().returns(false)
    CfgSeqImpl(list).isEmpty shouldBe false
  }

  "size" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(25)
    CfgSeqImpl(list).size shouldBe 25
  }

  "knownSize" should "return the same value as underlying.size" in {
    val list = mock[ConfigList]
    (list.size _).expects().returns(1024)
    CfgSeqImpl(list).knownSize shouldBe 1024
  }

  "origin" should "be a wrap on underlying origin object" in {
    val list = mock[ConfigList]
    val expectedOrigin = mock[ConfigOrigin]
    (list.origin _).expects().returns(expectedOrigin)
    inside(CfgSeqImpl(list).origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs expectedOrigin }
  }
}
