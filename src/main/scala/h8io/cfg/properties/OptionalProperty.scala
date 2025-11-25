package h8io.cfg.properties

import cats.syntax.all.*
import h8io.cfg.Property.Value
import h8io.cfg.raw.Node
import h8io.cfg.{Decoder, Property}

final case class OptionalProperty[+T: Decoder](name: String) extends Property[Option[T]] {
  def apply(cfg: Node.Map): Value[Option[T]] =
    cfg(name) match {
      case node: Node.Value => Decoder(node).map(Some(_))
      case _: Node.None | _: Node.Null => None.valid
    }
}
