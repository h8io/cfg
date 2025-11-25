package h8io

import h8io.cfg.raw.Node

package object cfg {
  type Decoder[+T] = Node.Value => Decoder.Result[T]

  implicit class DecoderOps[+T](private val decoder: Decoder[T]) extends AnyVal {
    def >=>[U](f: T => Decoder.Result[U]): Decoder[U] = decoder(_).andThen(f)
  }
}
