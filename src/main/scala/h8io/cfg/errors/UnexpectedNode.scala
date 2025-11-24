package h8io.cfg.errors

import h8io.cfg.Decoder
import h8io.cfg.raw.Node
import h8io.reflect.Type

final case class UnexpectedNode[T](node: Node.Value, tp: Type[T]) extends Decoder.Error
