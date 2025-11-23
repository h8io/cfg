package h8io.cfg

import cats.data.{Validated, ValidatedNec}
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import h8io.cfg.errors.Thrown
import h8io.cfg.raw.Node

import scala.util.control.NonFatal

trait Property[+T] extends (Node.Map => Property.Value[T]) {
  def name: String
  def apply(cfg: Node.Map): Property.Value[T]
}

object Property {
  type Value[+T] = ValidatedNec[CfgError.Any, T]

  def decode[T](node: Node.Value)(implicit decoder: Decoder[T]): Decoder.Result[T] =
    try decoder(node)
    catch {
      case NonFatal(e) => Thrown(node, e).invalidNec
    }

  implicit object Functor extends cats.Functor[Property] {
    override def map[A, B](fa: Property[A])(f: A => B): Property[B] =
      new Property[B] {
        override def name: String = fa.name
        override def apply(cfg: Node.Map): Value[B] =
          fa(cfg).andThen { value =>
            try Validated.Valid(f(value))
            catch {
              case NonFatal(e) => Thrown(cfg, e).invalidNec
            }
          }
      }
  }

  final case class Optional[+T: Decoder](name: String) extends Property[Option[T]] {
    def apply(cfg: Node.Map): Value[Option[T]] =
      cfg(name) match {
        case node: Node.Value => decode(node).map(Some(_))
        case _: Node.None | _: Node.Null => Validated.Valid(None)
      }
  }

  final case class Mandatory[+T: Decoder](name: String) extends Property[T] {
    def apply(cfg: Node.Map): Value[T] =
      cfg(name) match {
        case node: Node.Value => decode(node)
        case node: CfgError.Any => node.invalidNec
      }
  }
}
