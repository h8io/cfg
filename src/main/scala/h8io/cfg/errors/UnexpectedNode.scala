package h8io.cfg.errors

import h8io.cfg.CfgError
import h8io.cfg.raw.{Id, Node}
import h8io.reflect.Type

final case class UnexpectedNode[T](node: Node.Value[Id], tp: Type[T]) extends CfgError
