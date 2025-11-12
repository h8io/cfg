package h8io.cfg

import cats.data.Validated
import h8io.cfg.errors.Missing
import h8io.cfg.raw.Node

trait Property[+T] extends Decoder[Node.Map, T] {
  def name: String

  def apply(cfg: Node.Map): DecoderResult[T]
}

object Property {
  def fromDecoder[T](name: String)(implicit decoder: Decoder[Node, T]): FromDecoder[T] = FromDecoder(name, decoder)

  final case class FromDecoder[+T](name: String, decoder: Decoder[Node, T]) extends Property[T] {
    def apply(cfg: Node.Map): DecoderResult[T] = decoder(cfg(name))
  }

  def optional[T](name: String)(implicit decoder: Decoder[Node.Some, T]): Optional[T] = Optional(name, decoder)

  final case class Optional[+T](name: String, decoder: Decoder[Node.Some, T]) extends Property[Option[T]] {
    def apply(cfg: Node.Map): DecoderResult[Option[T]] =
      cfg(name) match {
        case _: Node.None => Validated.Valid(None)
        case some: Node.Some => decoder(some).map(Some(_))
      }
  }

  def mandatory[T](name: String)(implicit decoder: Decoder[Node.Some, T]): Mandatory[T] = Mandatory(name, decoder)

  final case class Mandatory[+T](name: String, decoder: Decoder[Node.Some, T]) extends Property[T] {
    def apply(cfg: Node.Map): DecoderResult[T] =
      cfg(name) match {
        case Node.None(id, origin) => Validated.invalidNec(Missing(id, origin))
        case some: Node.Some => decoder(some)
      }
  }
}
