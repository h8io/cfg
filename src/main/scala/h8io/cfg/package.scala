package h8io

import cats.Functor
import cats.data.ValidatedNec
import h8io.cfg.raw.Node

package object cfg {
  type DecoderResult[+T] = ValidatedNec[CfgError, T]

  type Decoder[-N <: Node.Some, +T] = N => DecoderResult[T]

  implicit def DecoderFunctor[N <: Node.Some]: Functor[λ[T => Decoder[N, T]]] =
    new Functor[λ[T => Decoder[N, T]]] {
      override def map[A, B](fa: Decoder[N, A])(f: A => B): Decoder[N, B] = fa(_).map(f)
    }
}
