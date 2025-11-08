package h8io

import cats.data.ValidatedNec
import h8io.cfg.raw.Node

package object cfg {
  type DecoderResult[+T] = ValidatedNec[DecoderError, T]

  type Decoder[-N <: Node[?], +T] = N => DecoderResult[T]
}
