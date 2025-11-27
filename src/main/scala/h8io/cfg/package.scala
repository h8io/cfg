package h8io

import cats.data.Validated
import h8io.cfg.raw.Node

package object cfg {
  type Decoder[+T] = Node.Value => Decoder.Result[T]

  implicit class DecoderOps[T](private val self: Decoder[T]) extends AnyVal {
    def >=>[U](f: T => Decoder.Result[U]): Decoder[U] = self(_).andThen(f)

    def ||(other: Decoder[T]): Decoder[T] =
      node =>
        self(node) match {
          case v @ Validated.Valid(_) => v
          case Validated.Invalid(leftErrors) => other(node) match {
              case v @ Validated.Valid(_) => v
              case Validated.Invalid(rightErrors) => Validated.Invalid(leftErrors ++ rightErrors)
            }
        }

  }
}
