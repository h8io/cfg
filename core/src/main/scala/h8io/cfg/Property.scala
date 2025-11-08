package h8io.cfg

import cats.data.Validated
import h8io.cfg.errors.Missing
import h8io.cfg.raw.{Id, Node}

trait Property[+T] extends Decoder[Node.Map[Id], T] {
  def name: String

  def apply(cfg: Node.Map[Id]): DecoderResult[T]
}

object Property {
  def fromDecoder[T](name: String)(implicit decoder: UniversalDecoder[T]): FromDecoder[T] = FromDecoder(name, decoder)

  final case class FromDecoder[+T](name: String, decoder: UniversalDecoder[T]) extends Property[T] {
    def apply(cfg: Node.Map[Id]): DecoderResult[T] = decoder(cfg(name))
  }

  def optional[T](name: String)(implicit decoder: Decoder[Node.Some[Id], T]): Optional[T] = Optional(name, decoder)

  final case class Optional[+T](name: String, decoder: Decoder[Node.Some[Id], T]) extends Property[Option[T]] {
    def apply(cfg: Node.Map[Id]): DecoderResult[Option[T]] =
      cfg(name) match {
        case _: Node.None[?] => Validated.Valid(None)
        case some: Node.Some[Id] => decoder(some).map(Some(_))
      }
  }

  def mandatory[T](name: String)(implicit decoder: Decoder[Node.Some[Id], T]): Mandatory[T] = Mandatory(name, decoder)

  final case class Mandatory[+T](name: String, decoder: Decoder[Node.Some[Id], T]) extends Property[T] {
    def apply(cfg: Node.Map[Id]): DecoderResult[T] =
      cfg(name) match {
        case Node.None(id, origin) => Validated.invalidNec(Missing(id, origin))
        case some: Node.Some[Id] => decoder(some)
      }
  }
}
