package h8io

import cats.data.{Kleisli, ValidatedNec}

package object cfg {
  type CfgResult[+T] = ValidatedNec[CfgError, T]

  type Decoder[T] = Kleisli[CfgResult, CfgValue, T]

  object Decoder {
    def apply[T](f: CfgValue => CfgResult[T]): Decoder[T] = Kleisli(f)
  }
}
