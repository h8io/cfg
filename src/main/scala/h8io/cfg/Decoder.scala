package h8io.cfg

import cats.data.{Validated, ValidatedNec}
import h8io.cfg.errors.Thrown
import h8io.cfg.raw.Node

import scala.annotation.tailrec
import scala.util.control.NonFatal

trait Decoder[+T] extends (Node.Value => Decoder.Result[T]) {
  def apply(node: Node.Value): Decoder.Result[T]

  def safe: Decoder.Safe[T]

  final def map[U](f: T => U): Decoder.Unsafe[U] = node => apply(node).map(f)
}

object Decoder {
  type Result[+T] = ValidatedNec[DecoderError, T]

  trait Safe[+T] extends Decoder[T] {
    final def safe: Decoder.Safe[T] = this
  }

  trait Unsafe[+T] extends Decoder[T] {
    final def safe: Safe[T] =
      (node: Node.Value) =>
        try apply(node)
        catch {
          case NonFatal(e) => Validated.invalidNec(Thrown(node, e))
        }
  }

  implicit object Monad extends cats.Monad[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.map(f)

    override def pure[A](x: A): Decoder.Safe[A] = _ => Validated.Valid(x)

    override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder.Unsafe[B] =
      node =>
        fa(node) match {
          case Validated.Valid(a) => f(a)(node)
          case invalid @ Validated.Invalid(_) => invalid
        }

    override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder.Unsafe[B] = { node =>
      @tailrec def loop(current: A): Result[B] =
        f(current)(node) match {
          case Validated.Valid(Right(b)) => Validated.Valid(b)
          case Validated.Valid(Left(next)) => loop(next)
          case invalid @ Validated.Invalid(_) => invalid
        }
      loop(a)
    }
  }
}
