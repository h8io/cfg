package h8io

import cats.Functor
import cats.data.ValidatedNec
import h8io.cfg.raw.{Id, Node}

package object cfg {
  type DecoderResult[+T] = ValidatedNec[DecoderError, T]

  type Decoder[-N <: Node[Id], +T] = N => DecoderResult[T]

  type UniversalDecoder[+T] = Decoder[Node[Id], T]

  implicit def decoderOps[N <: Node[Id]]: Functor[λ[T => Decoder[N, T]]] = DecoderOps[N]
}
