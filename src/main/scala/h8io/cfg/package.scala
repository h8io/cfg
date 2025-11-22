package h8io

import cats.Monad
import cats.data.{Validated, ValidatedNec}
import h8io.cfg.raw.Node

import scala.annotation.tailrec

package object cfg {
  type DecoderResult[+T] = ValidatedNec[DecoderError, T]

  type PropertyValue[+T] = ValidatedNec[CfgError, T]

  type Decoder[+T] = Node.Value => DecoderResult[T]

  implicit object DecoderMonad extends Monad[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.andThen(_.map(f))

    override def pure[A](x: A): Decoder[A] = _ => Validated.Valid(x)

    override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] =
      node =>
        fa(node) match {
          case Validated.Valid(a) => f(a)(node)
          case invalid @ Validated.Invalid(_) => invalid
        }

    override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = { node =>
      @tailrec def loop(current: A): DecoderResult[B] =
        f(current)(node) match {
          case Validated.Valid(Right(b)) => Validated.Valid(b)
          case Validated.Valid(Left(next)) => loop(next)
          case invalid @ Validated.Invalid(_) => invalid
        }
      loop(a)
    }
  }
}
