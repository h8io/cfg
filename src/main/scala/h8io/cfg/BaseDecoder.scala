package h8io.cfg

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.{Id, Node}
import h8io.reflect.Type

abstract class BaseDecoder[+T: Type] extends Decoder[T] {
  final def apply(node: Node.Value[Id]): Decoder.Result[T] =
    node match {
      case scalar: Node.Scalar[Id] => parse(scalar)
      case map: Node.Map[Id] => parse(map)
      case seq: Node.Seq[Id] => parse(seq)
    }

  private def unexpected(node: Node.Value[Id]): Decoder.Result[T] = UnexpectedNode(node, implicitly[Type[T]]).invalidNec

  protected def parse(scalar: Node.Scalar[Id]): Decoder.Result[T] = unexpected(scalar)
  protected def parse(map: Node.Map[Id]): Decoder.Result[T] = unexpected(map)
  protected def parse(seq: Node.Seq[Id]): Decoder.Result[T] = unexpected(seq)
}
