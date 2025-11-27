package h8io.cfg

import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*
import cats.{Monad, SemigroupK}
import h8io.cfg.raw.Node

import scala.annotation.tailrec
import scala.util.control.NonFatal

object Decoder {
  type Result[+T] = ValidatedNec[CfgError, T]

  final case class Thrown(node: Node.Value, cause: Throwable) extends CfgError

  def apply[T](node: Node.Value)(implicit decoder: Decoder[T]): Decoder.Result[T] =
    try decoder(node)
    catch {
      case NonFatal(e) => Decoder.Thrown(node, e).invalidNec
    }

  implicit val monad: Monad[Decoder] = new Monad[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.andThen(_.map(f))

    override def pure[A](x: A): Decoder[A] = _ => x.valid

    override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] =
      node =>
        fa(node) match {
          case Validated.Valid(a) => f(a)(node)
          case invalid @ Validated.Invalid(_) => invalid
        }

    override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = { node =>
      @tailrec def loop(current: A): Result[B] =
        f(current)(node) match {
          case Validated.Valid(Right(b)) => b.valid
          case Validated.Valid(Left(next)) => loop(next)
          case invalid @ Validated.Invalid(_) => invalid
        }
      loop(a)
    }
  }

  implicit val semigroupK: SemigroupK[Decoder] = new SemigroupK[Decoder] {
    override def combineK[A](x: Decoder[A], y: Decoder[A]): Decoder[A] =
      node =>
        x(node) match {
          case v @ Validated.Valid(_) => v
          case Validated.Invalid(xe) => y(node) match {
              case v @ Validated.Valid(_) => v
              case Validated.Invalid(ye) => Validated.Invalid(xe ++ ye)
            }
        }
  }
}
