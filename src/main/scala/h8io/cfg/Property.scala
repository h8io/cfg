package h8io.cfg

import cats.data.ValidatedNec
import cats.syntax.all.*
import h8io.cfg.raw.Node

import scala.util.control.NonFatal

trait Property[+T] extends (Node.Map => Property.Value[T]) {
  def name: String
  def apply(cfg: Node.Map): Property.Value[T]
}

object Property {
  type Value[+T] = ValidatedNec[CfgError.Any, T]

  trait Error extends CfgError[Node.Map] {
    def property: Property[?]
  }

  final case class Thrown(node: Node.Map, property: Property[?], cause: Throwable) extends Error

  def decode[T](node: Node.Value)(implicit decoder: Decoder[T]): Decoder.Result[T] =
    try decoder(node)
    catch {
      case NonFatal(e) => Decoder.Thrown(node, e).invalidNec
    }

  implicit object Functor extends cats.Functor[Property] {
    override def map[A, B](fa: Property[A])(f: A => B): Property[B] =
      new Property[B] {
        override def name: String = fa.name
        override def apply(cfg: Node.Map): Value[B] =
          fa(cfg).andThen { value =>
            try f(value).valid
            catch {
              case NonFatal(e) => Thrown(cfg, fa, e).invalidNec
            }
          }
      }
  }
}
