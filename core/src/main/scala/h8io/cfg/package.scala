package h8io

import cats.Functor
import cats.data.{Validated, ValidatedNec}
import h8io.cfg.errors.Thrown
import h8io.cfg.raw.{Id, Node}

import scala.util.control.NonFatal

package object cfg {
  type DecoderResult[+T] = ValidatedNec[DecoderError, T]

  type Decoder[-N <: Node[Id], +T] = N => DecoderResult[T]

  implicit def decoderFunctor[N <: Node[Id]]: Functor[Decoder[N, *]] =
    new Functor[Decoder[N, *]] {
      override def map[A, B](fa: Decoder[N, A])(f: A => B): Decoder[N, B] =
        node =>
          try fa(node).map(f)
          catch {
            case NonFatal(e) => Validated.invalidNec(Thrown(node.id, node.origin, e))
          }
    }
}
