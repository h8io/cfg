package h8io.cfg.errors

import h8io.cfg.DecoderError
import h8io.cfg.raw.{Id, Origin}

final case class Thrown(id: Id, origin: Origin, cause: Throwable) extends DecoderError
