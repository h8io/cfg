package h8io.cfg

import h8io.cfg.raw.{Id, Origin}

trait DecoderError {
  val id: Id
  val origin: Origin
}
