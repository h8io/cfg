package h8io.cfg.impl.hocon

import h8io.cfg.impl.hocon.context.CfgContext
import h8io.cfg.{CfgMap, CfgNull, CfgScalar, CfgSeq}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WrapTest extends AnyFlatSpec with Matchers with Inside {
  private val config = hocon"map { a: x, b: y, c: z, d: null }, seq: [1, 2, 3, null], scalar: 42, null: null"

  "Wrap" should "create a CfgMap object" in {
    val mapValue = config.get("map")
    inside(Wrap(mapValue)) { case map: CfgMap =>
      map.iterator.map {
        inside(_) {
          case (k, CfgScalar(value, _)) => k -> Some(value)
          case (k, CfgNull(_)) => k -> None
        }
      }.toList should contain theSameElementsAs
        List("a" -> Some("x"), "b" -> Some("y"), "c" -> Some("z"), "d" -> None)
      inside(map.origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs mapValue.origin }
    }
  }

  it should "create a CfgSeq object" in {
    val seqValue = config.get("seq")
    inside(Wrap(seqValue)) { case seq: CfgSeq =>
      seq.iterator.map(
        inside(_) {
          case CfgScalar(value, _) => Some(value)
          case CfgNull(_) => None
        }).toList should contain theSameElementsInOrderAs
        List(Some("1"), Some("2"), Some("3"), None)
      inside(seq.origin) { case CfgOriginImpl(origin) => origin should be theSameInstanceAs seqValue.origin }
    }
  }

  it should "create a CfgScalar object" in {
    val scalarValue = config.get("scalar")
    inside(Wrap(scalarValue)) { case CfgScalar("42", CfgOriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a CfgNull object" in {
    val nullValue = config.get("null")
    Wrap(nullValue) shouldBe CfgNull(CfgOriginImpl(nullValue.origin))
    inside(Wrap(nullValue)) { case CfgNull(CfgOriginImpl(nullOrigin)) =>
      nullOrigin should be theSameInstanceAs nullValue.origin
    }
  }
}
