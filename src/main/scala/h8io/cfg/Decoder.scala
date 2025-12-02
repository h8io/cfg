package h8io.cfg

import cats.data.Validated
import cats.syntax.all.*
import cats.{Monad, SemigroupK}
import h8io.cfg.raw.Node

import scala.annotation.tailrec
import scala.util.control.NonFatal

object Decoder {
  final case class Thrown(node: Node.Value, cause: Throwable) extends NodeError

  def apply[T](node: Node.Value)(implicit decoder: Decoder[T]): CfgValue[T] =
    try decoder(node)
    catch {
      case NonFatal(e) => Decoder.Thrown(node, e).invalid
    }

  implicit val monad: Monad[Decoder] = new Monad[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.andThen(_.map(f))

    def pure[A](x: A): Decoder[A] = _ => x.valid

    def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] = fa >=> f

    def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = { node =>
      @tailrec def loop(current: A): CfgValue[B] =
        f(current)(node) match {
          case Validated.Valid(Right(b)) => b.valid
          case Validated.Valid(Left(next)) => loop(next)
          case invalid @ Validated.Invalid(_) => invalid
        }
      loop(a)
    }
  }

  implicit val semigroupK: SemigroupK[Decoder] = new SemigroupK[Decoder] {
    def combineK[A](x: Decoder[A], y: Decoder[A]): Decoder[A] = x || y
  }
}
