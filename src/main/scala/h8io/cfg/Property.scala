package h8io.cfg

import cats.data.ValidatedNec
import cats.syntax.all.*
import h8io.cfg.raw.{Id, Node}

import scala.util.control.NonFatal

trait Property[+T] extends (Node.Map[Id] => Property.Value[T]) {
  self =>

  def name: String
  def apply(cfg: Node.Map[Id]): Property.Value[T]

  final def >=>[U](f: T => Property.Value[U]): Property[U] =
    new Property[U] {
      override def name: String = self.name
      override def apply(cfg: Node.Map[Id]): Property.Value[U] =
        self(cfg).andThen { value =>
          try f(value)
          catch {
            case NonFatal(e) => Property.Thrown(cfg, this, e).invalidNec
          }
        }
    }
}

object Property {
  type Value[+T] = ValidatedNec[CfgError, T]

  final case class Thrown(node: Node.Map[Id], property: Property[?], cause: Throwable) extends CfgError

  implicit object Functor extends cats.Functor[Property] {
    override def map[A, B](fa: Property[A])(f: A => B): Property[B] =
      new Property[B] {
        override def name: String = fa.name
        override def apply(cfg: Node.Map[Id]): Value[B] =
          fa(cfg).andThen { value =>
            try f(value).valid
            catch {
              case NonFatal(e) => Thrown(cfg, fa, e).invalidNec
            }
          }
      }
  }
}
