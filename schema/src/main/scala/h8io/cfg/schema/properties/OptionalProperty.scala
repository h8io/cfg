package h8io.cfg.schema.properties

import cats.syntax.all.*
import h8io.cfg.{Id, Node}
import h8io.cfg.schema.{CfgValue, Decoder, Property}

final case class OptionalProperty[+T: Decoder](name: String) extends Property[Option[T]] {
  def apply(cfg: Node.Map): CfgValue[Option[T]] =
    cfg(name) match {
      case node: Node.IValue[Id.Key] => Decoder(node).map(Some(_))
      case _: Node.INone[Id.Key] | _: Node.INull[Id.Key] => None.valid
    }
}
