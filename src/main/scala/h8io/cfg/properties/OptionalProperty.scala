package h8io.cfg.properties

import cats.syntax.all.*
import h8io.cfg.Property.Value
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.{Decoder, Property}

final case class OptionalProperty[+T: Decoder](name: String) extends Property[Option[T]] {
  def apply(cfg: Node.Map): Value[Option[T]] =
    cfg(name) match {
      case node: Node.IValue[Id.Key] => Decoder(node).map(Some(_))
      case _: Node.INone[Id.Key] | _: Node.INull[Id.Key] => None.valid
    }
}
