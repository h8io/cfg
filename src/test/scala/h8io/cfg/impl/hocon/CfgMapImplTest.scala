package h8io.cfg.impl.hocon

import h8io.cfg.impl.hocon.context.CfgContext
import h8io.cfg.{CfgMap, CfgNone, CfgNull, CfgScalar, CfgSeq}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CfgMapImplTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "apply" should "return a CfgScalar" in {
    val obj = hocon"""scalar: 42"""
    val scalar = obj.get("scalar")
    inside(CfgMapImpl(obj)("scalar")) { case CfgScalar("42", CfgOriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a CfgNull" in {
    val obj = hocon"""scalar: null"""
    val scalar = obj.get("scalar")
    inside(CfgMapImpl(obj)("scalar")) { case CfgNull(CfgOriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalar.origin
    }
  }

  it should "return a CfgNone" in {
    val obj = hocon"""scalar: 13"""
    inside(CfgMapImpl(obj)("unexistent")) { case CfgNone(CfgOriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs obj.origin
    }
  }

  it should "return a CfgSeq" in {
    val obj = hocon"""seq: [a, null, b, null, c, null, "null"]"""
    val list = obj.toConfig.getList("seq")
    inside(CfgMapImpl(obj)("seq")) { case seq: CfgSeq =>
      seq.iterator.zipWithIndex.map { case (value, i) =>
        val expectedOrigin = list.get(i).origin
        inside(value) {
          case CfgScalar(value, CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            Some(value)
          case CfgNull(CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("a"), None, Some("b"), None, Some("c"), None, Some("null"))
      inside(seq.origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs list.origin }
    }
  }

  it should "return a CfgMap" in {
    val cfg = hocon"""map { a: null, b: c, null: "null" }"""
    val obj = cfg.toConfig.getObject("map")
    inside(CfgMapImpl(cfg)("map")) { case map: CfgMap =>
      map.iterator.map { case (key, value) =>
        val expectedOrigin = obj.get(key).origin
        inside(value) {
          case CfgScalar(value, CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            key -> Some(value)
          case CfgNull(CfgOriginImpl(origin)) =>
            origin should be theSameInstanceAs expectedOrigin
            key -> None
        }
      }.toList should contain theSameElementsAs List("a" -> None, "b" -> Some("c"), "null" -> Some("null"))
      inside(map.origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs obj.origin }
    }
  }
}
