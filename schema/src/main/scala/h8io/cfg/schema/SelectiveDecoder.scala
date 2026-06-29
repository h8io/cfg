package h8io.cfg.schema

import cats.syntax.all.*
import h8io.cfg.schema.errors.UnexpectedNode
import h8io.cfg.Node
import izumi.reflect.Tag

abstract class SelectiveDecoder[+T: Tag] extends Decoder[T] {
  final def apply(node: Node.Value): CfgValue[T] =
    node match {
      case scalar: Node.Scalar => parse(scalar)
      case map: Node.Map => parse(map)
      case seq: Node.Seq => parse(seq)
    }

  private def unexpected(node: Node.Value): CfgValue[T] = UnexpectedNode[T](node).invalid

  protected def parse(scalar: Node.Scalar): CfgValue[T] = unexpected(scalar)
  protected def parse(map: Node.Map): CfgValue[T] = unexpected(map)
  protected def parse(seq: Node.Seq): CfgValue[T] = unexpected(seq)
}
