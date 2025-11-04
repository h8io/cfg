package h8io.cfg.raw.hocon

import context.CfgContext
import h8io.cfg.raw.{Entry, Ref}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WrapTest extends AnyFlatSpec with Matchers with Inside {
  private val config = hocon"map { a: x, b: y, c: z, d: null }, seq: [1, 2, 3, null], scalar: 42, null: null"

  "Wrap" should "create a CfgMap object" in {
    val mapValue = config.get("map")
    val rootPath = Ref.Index(42)
    inside(Wrap(rootPath, mapValue)) { case map: Entry.Map[?] =>
      map.iterator.map {
        inside(_) {
          case Entry.Scalar(path, value, _) => path.key -> Some(value)
          case Entry.Null(path, _) => path.key -> None
        }
      }.toList should contain theSameElementsAs
        List("a" -> Some("x"), "b" -> Some("y"), "c" -> Some("z"), "d" -> None)
      inside(map.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs mapValue.origin }
    }
  }

  it should "create a CfgSeq object" in {
    val seqValue = config.get("seq")
    inside(Wrap(Ref.Root, seqValue)) { case seq: Entry.Seq[?] =>
      seq.iterator.zipWithIndex.map { case (entry, i) =>
        inside(entry) {
          case Entry.Scalar(Ref.Index(`i`), value, _) => Some(value)
          case Entry.Null(Ref.Index(`i`), _) => None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("1"), Some("2"), Some("3"), None)
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs seqValue.origin }
    }
  }

  it should "create a CfgScalar object" in {
    val scalarValue = config.get("scalar")
    inside(Wrap(Ref.Root, scalarValue)) { case Entry.Scalar(Ref.Root, "42", OriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a CfgNull object" in {
    val nullValue = config.get("null")
    inside(Wrap(Ref.Root, nullValue)) { case Entry.Null(Ref.Root, OriginImpl(origin)) =>
      origin should be theSameInstanceAs nullValue.origin
    }
  }
}
