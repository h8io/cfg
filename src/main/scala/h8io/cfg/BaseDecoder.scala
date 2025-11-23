package h8io.cfg

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.Node
import h8io.reflect.Type

abstract class BaseDecoder[+T: Type] extends Decoder[T] {
  final def apply(node: Node.Value): Decoder.Result[T] =
    node match {
      case scalar: Node.Scalar => parse(scalar)
      case map: Node.Map => parse(map)
      case seq: Node.Seq => parse(seq)
    }

  private def unexpected(node: Node.Value): Decoder.Result[T] = UnexpectedNode(node, implicitly[Type[T]]).invalidNec[T]

  protected def parse(scalar: Node.Scalar): Decoder.Result[T] = unexpected(scalar)
  protected def parse(map: Node.Map): Decoder.Result[T] = unexpected(map)
  protected def parse(seq: Node.Seq): Decoder.Result[T] = unexpected(seq)
}
