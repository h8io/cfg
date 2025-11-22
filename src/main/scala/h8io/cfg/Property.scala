package h8io.cfg

import cats.data.Validated
import h8io.cfg.errors.Thrown
import h8io.cfg.raw.Node

trait Property[+T] extends (Node.Map => PropertyValue[T]) {
  def name: String
  def apply(cfg: Node.Map): PropertyValue[T]
}

object Property {
  def optional[T](name: String)(implicit decoder: Decoder[T]): Optional[T] = Optional(name, decoder)

  final case class Optional[+T](name: String, decoder: Decoder[T]) extends Property[Option[T]] {
    def apply(cfg: Node.Map): PropertyValue[Option[T]] =
      cfg(name) match {
        case node: Node.Value => safeApply(decoder, node).map(Some(_))
        case _: Node.None | _: Node.Null => Validated.Valid(None)
      }
  }

  def mandatory[T](name: String)(implicit decoder: Decoder[T]): Mandatory[T] = Mandatory(name, decoder)

  final case class Mandatory[+T](name: String, decoder: Decoder[T]) extends Property[T] {
    def apply(cfg: Node.Map): PropertyValue[T] =
      cfg(name) match {
        case node: Node.Value => safeApply(decoder, node)
        case node: CfgError => Validated.invalidNec(node)
      }
  }

  private def safeApply[T](decoder: Decoder[T], node: Node.Value): DecoderResult[T] =
    try decoder(node)
    catch {
      case e: Exception => Validated.invalidNec(Thrown(node, e))
    }
}
