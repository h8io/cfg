package h8io.cfg.errors

import h8io.cfg.DecoderError
import h8io.cfg.raw.{Id, Node}

final case class Missing(id: Id, node: Node) extends DecoderError
