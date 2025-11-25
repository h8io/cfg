package h8io.cfg.raw.hocon

import h8io.cfg.raw.hocon.context.CfgContext
import h8io.cfg.raw.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WrapTest extends AnyFlatSpec with Matchers with Inside {
  private val config = hocon"map { a: x, b: y, c: z, d: null }, seq: [1, 2, 3, null], scalar: 42, null: null"

  "wrap" should "create a Node.Map object" in {
    val mapValue = config.get("map")
    val rootId = Id.Index(42, Id.Root)
    inside(wrap(rootId, mapValue)) { case map: Node.IMap[Id.Index] =>
      map.iterator.map {
        inside(_) {
          case Node.Scalar(Id.Key(key, `rootId`), scalar, _) => key -> Some(scalar)
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
      seq.iterator.zipWithIndex.map { case (node, i) =>
        val id = seq.id
        inside(node) {
          case Node.Scalar(Id.Index(`i`, `id`), scalar, _) => Some(scalar)
          case Node.Null(Id.Index(`i`, `id`), _) => None
        }
      }.toList should contain theSameElementsInOrderAs
        List(Some("1"), Some("2"), Some("3"), None)
      inside(seq.location) { case LocationImpl(origin) => origin should be theSameInstanceAs seqValue.origin }
    }
  }

  it should "create a Node.Scalar object" in {
    val scalarValue = config.get("scalar")
    inside(wrap(Id.Root, scalarValue)) { case Node.Scalar(Id.Root, "42", LocationImpl(scalarOrigin)) =>
      scalarOrigin should be theSameInstanceAs scalarValue.origin
    }
  }

  it should "create a Node.Null object" in {
    val nullValue = config.get("null")
    inside(wrap(Id.Root, nullValue)) { case Node.Null(Id.Root, LocationImpl(origin)) =>
      origin should be theSameInstanceAs nullValue.origin
    }
  }
}
