package h8io.cfg

import cats.data.{Validated, ValidatedNec}

import scala.annotation.tailrec

object Decoder {
  type Result[+T] = ValidatedNec[DecoderError, T]

  implicit object Monad extends cats.Monad[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.andThen(_.map(f))

    override def pure[A](x: A): Decoder[A] = _ => Validated.Valid(x)

    override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] =
      node =>
        fa(node) match {
          case Validated.Valid(a) => f(a)(node)
          case invalid @ Validated.Invalid(_) => invalid
        }

    override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = { node =>
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
