package h8io

import cats.data.ValidatedNec

package object cfg {
  type CfgResult[+T] = ValidatedNec[CfgError, T]
}
