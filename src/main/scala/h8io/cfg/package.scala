package h8io

import h8io.cfg.raw.Node

package object cfg {
  type Decoder[+T] = Node.Value => Decoder.Result[T]
}
