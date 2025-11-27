package h8io.cfg.properties

import cats.syntax.all.*
import h8io.cfg.Property.Value
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.{Decoder, Property}

final case class MandatoryProperty[+T](name: String)(implicit decoder: Decoder[T]) extends Property[T] {
  def apply(cfg: Node.Map): Value[T] =
    cfg(name) match {
      case node: Node.Value => Decoder(node)
      case node: Node.INull[Id.Key] => node.invalidNec
      case node: Node.INone[Id.Key] => node.invalidNec
    }
}
