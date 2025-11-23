package h8io.cfg

import cats.data.{Validated, ValidatedNec}
import h8io.cfg.raw.Node

trait Property[+T] extends (Node.Map => Property.Value[T]) {
  def name: String
  def apply(cfg: Node.Map): Property.Value[T]
}

object Property {
  type Value[+T] = ValidatedNec[CfgError, T]

  def optional[T](name: String)(implicit decoder: Decoder[T]): Optional[T] = Optional(name, decoder.safe)

  final case class Optional[+T](name: String, decoder: Decoder.Safe[T]) extends Property[Option[T]] {
    def apply(cfg: Node.Map): Value[Option[T]] =
      cfg(name) match {
        case node: Node.Value => decoder.safe(node).map(Some(_))
        case _: Node.None | _: Node.Null => Validated.Valid(None)
      }
  }

  def mandatory[T](name: String)(implicit decoder: Decoder[T]): Mandatory[T] = Mandatory(name, decoder.safe)

  final case class Mandatory[+T](name: String, decoder: Decoder.Safe[T]) extends Property[T] {
    def apply(cfg: Node.Map): Value[T] =
      cfg(name) match {
        case node: Node.Value => decoder.safe(node)
        case node: CfgError => Validated.invalidNec(node)
      }
  }
}
