package h8io.cfg.errors

import h8io.cfg.NodeError
import h8io.cfg.raw.Node
import h8io.reflect.Type

final class UnexpectedNode[T](val node: Node.Value, val tp: Type[T]) extends NodeError {
  override def hashCode(): Int = node.hashCode() * 31 + tp.hashCode()

  override def equals(obj: Any): Boolean =
    obj match {
      case other: UnexpectedNode[?] => tp == other.tp && node == other.node
      case _ => false
    }
}

object UnexpectedNode {
  def apply[T](node: Node.Value)(implicit tp: Type[T]): UnexpectedNode[T] = new UnexpectedNode[T](node, tp)
}
