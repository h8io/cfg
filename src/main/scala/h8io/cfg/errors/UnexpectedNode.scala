package h8io.cfg.errors

import h8io.cfg.CfgError
import h8io.cfg.raw.Node
import h8io.reflect.Type

final class UnexpectedNode[T](val node: Node.Value, val tp: Type[T]) extends CfgError {
  override def hashCode(): Int = node.hashCode() * 17 + tp.hashCode()

  override def equals(obj: Any): Boolean =
    obj match {
      case that: UnexpectedNode[?] => node == that.node && tp == that.tp
      case _ => false
    }
}

object UnexpectedNode {
  def apply[T](node: Node.Value)(implicit tp: Type[T]): UnexpectedNode[T] = new UnexpectedNode[T](node, tp)
}
