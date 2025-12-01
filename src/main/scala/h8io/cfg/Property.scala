package h8io.cfg

import cats.Functor
import cats.syntax.all.*
import h8io.cfg.raw.Node

import scala.util.control.NonFatal

trait Property[+T] extends (Node.Map => CfgValue[T]) {
  self =>

  def name: String
  def apply(cfg: Node.Map): CfgValue[T]

  final def >=>[U](f: T => CfgValue[U]): Property[U] =
    new Property[U] {
      override def name: String = self.name
      override def apply(cfg: Node.Map): CfgValue[U] =
        self(cfg).andThen { value =>
          try f(value)
          catch {
            case NonFatal(e) => Property.Thrown(cfg, this, e).invalid
          }
        }
    }
}

object Property {
  final case class Thrown(node: Node.Map, property: Property[?], cause: Throwable) extends NodeError

  implicit val functor: Functor[Property] = new Functor[Property] {
    override def map[A, B](fa: Property[A])(f: A => B): Property[B] =
      new Property[B] {
        override def name: String = fa.name
        override def apply(cfg: Node.Map): CfgValue[B] =
          fa(cfg).andThen { value =>
            try f(value).valid
            catch {
              case NonFatal(e) => Thrown(cfg, fa, e).invalid
            }
          }
      }
  }
}
