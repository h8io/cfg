package h8io.cfg

import cats.data.Validated
import h8io.cfg.fluent.errors.CfgErrorOps

import scala.collection.mutable

package object fluent {
  type CfgValue[+T] = Validated[CfgError, T]

  type Decoder[+T] = Node.Value => CfgValue[T]

  implicit class ValuesOps[T](private val self: Iterator[CfgValue[T]]) extends AnyVal {
    def collectInto[C, B <: mutable.Builder[T, C]](builder: B): CfgValue[C] =
      (self foldLeft (Validated.valid(builder): CfgValue[B])) { (acc, value) =>
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
