package h8io.cfg.raw.hocon

import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WrapTest extends AnyFlatSpec with Matchers with Inside {
  private val config = hocon"map { a: x, b: y, c: z, d: null }, seq: [1, 2, 3, null], scalar: 42, null: null"

  "Wrap" should "create a Node.Map object" in {
    val mapValue = config.get("map")
    val rootId = Id.Index(42)
    inside(Wrap(rootId, mapValue)) { case map: Node.Map[?] =>
      map.iterator.map {
        inside(_) {
          case Node.Scalar(id, value, _) => id.key -> Some(value)
          case Node.Null(id, _) => id.key -> None
        }
      }.toList should contain theSameElementsAs
        List("a" -> Some("x"), "b" -> Some("y"), "c" -> Some("z"), "d" -> None)
      inside(map.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs mapValue.origin }
    }
  }

  it should "create a Node.Seq object" in {
    val seqValue = config.get("seq")
    inside(Wrap(Id.Root, seqValue)) { case seq: Node.Seq[?] =>
      seq.iterator.zipWithIndex.map { case (node, i) =>
        inside(node) {
          case Node.Scalar(Id.Index(`i`), value, _) => Some(value)
          case Node.Null(Id.Index(`i`), _) => None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("1"), Some("2"), Some("3"), None)
      inside(seq.origin) { case OriginImpl(origin) => origin should be theSameInstanceAs seqValue.origin }
    }
  }

  it should "create a Node.Scalar object" in {
    val scalarValue = config.get("scalar")
    inside(Wrap(Id.Root, scalarValue)) { case Node.Scalar(Id.Root, "42", OriginImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a Node.Null object" in {
    val nullValue = config.get("null")
    inside(Wrap(Id.Root, nullValue)) { case Node.Null(Id.Root, OriginImpl(origin)) =>
      origin should be theSameInstanceAs nullValue.origin
    }
  }
}
