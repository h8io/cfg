package h8io.cfg.schema.errors

import h8io.cfg.{Node, NodeError}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

final class UnexpectedNode[T](val node: Node.Value, val tp: LightTypeTag) extends NodeError {
  override def hashCode(): Int = node.hashCode() * 31 + tp.hashCode()

  override def equals(obj: Any): Boolean =
    obj match {
      case other: UnexpectedNode[?] => tp == other.tp && node == other.node
      case _ => false
    }
}

object UnexpectedNode {
  def apply[T: Tag](node: Node.Value): UnexpectedNode[T] = new UnexpectedNode[T](node, Tag[T].tag)
}
