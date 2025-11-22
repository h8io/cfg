package h8io

import cats.Functor
import cats.data.ValidatedNec
import h8io.cfg.raw.Node

package object cfg {
  type DecoderResult[+T] = ValidatedNec[DecoderError, T]

  type PropertyValue[+T] = ValidatedNec[CfgError, T]

  type Decoder[+T] = Node.Value => DecoderResult[T]

  implicit object DecoderFunctor extends Functor[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.andThen(_.map(f))
  }
}
