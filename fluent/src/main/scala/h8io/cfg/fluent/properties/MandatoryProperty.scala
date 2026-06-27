package h8io.cfg.fluent.properties

import cats.syntax.all.*
import h8io.cfg.{Id, Node}
import h8io.cfg.fluent.{CfgValue, Decoder, Property}

final case class MandatoryProperty[+T](name: String)(implicit decoder: Decoder[T]) extends Property[T] {
  def apply(cfg: Node.Map): CfgValue[T] =
    cfg(name) match {
      case node: Node.Value => Decoder(node)
      case node: Node.INull[Id.Key] => node.invalid
      case node: Node.INone[Id.Key] => node.invalid
    }
}
