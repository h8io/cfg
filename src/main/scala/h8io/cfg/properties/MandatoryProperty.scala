package h8io.cfg.properties

import cats.syntax.all.*
import h8io.cfg.Property.Value
import h8io.cfg.raw.Node
import h8io.cfg.{CfgError, Decoder, Property}

final case class MandatoryProperty[+T: Decoder](name: String) extends Property[T] {
  def apply(cfg: Node.Map): Value[T] =
    cfg(name) match {
      case node: Node.Value => Property.decode(node)
      case node: CfgError.Any => node.invalidNec
    }
}
