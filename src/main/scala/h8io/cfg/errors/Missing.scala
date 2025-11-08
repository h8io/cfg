package h8io.cfg.errors

import h8io.cfg.DecoderError
import h8io.cfg.raw.{Id, Origin}

final case class Missing(id: Id, origin: Origin) extends DecoderError
