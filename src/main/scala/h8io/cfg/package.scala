package h8io

import cats.data.Validated
import h8io.cfg.errors.CfgErrorOps
import h8io.cfg.raw.Node

import scala.collection.mutable

package object cfg {
  type CfgValue[+T] = Validated[CfgError, T]

  type Decoder[+T] = Node.Value => CfgValue[T]

  implicit class DecoderOps[T](private val self: Decoder[T]) extends AnyVal {
    def >=>[U](f: T => CfgValue[U]): Decoder[U] = self(_).andThen(f)

    def ||(other: Decoder[T]): Decoder[T] =
      node =>
        self(node) match {
          case v @ Validated.Valid(_) => v
          case Validated.Invalid(leftError) => other(node) match {
              case v @ Validated.Valid(_) => v
              case Validated.Invalid(rightError) => Validated.Invalid(leftError | rightError)
            }
        }
  }

  implicit class ValuesOps[T](private val self: Iterator[CfgValue[T]]) extends AnyVal {
    def sequence: CfgValue[Vector[T]] =
      (self foldLeft (Validated.valid(Vector.newBuilder[T]): CfgValue[mutable.Builder[T, Vector[T]]])) { (acc, value) =>
        (acc, value) match {
          case (Validated.Valid(values), Validated.Valid(value)) =>
            values += value
            acc
          case (Validated.Valid(_), invalid @ Validated.Invalid(_)) => invalid
          case (invalid @ Validated.Invalid(_), Validated.Valid(_)) => invalid
          case (Validated.Invalid(leftError), Validated.Invalid(rightError)) =>
            Validated.Invalid(leftError & rightError)
        }
      }.map(_.result())
  }
}
