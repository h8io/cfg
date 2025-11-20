package h8io.cfg.errors

import h8io.cfg.DecoderError
import h8io.cfg.raw.Node

final case class Thrown(node: Node, cause: Throwable) extends DecoderError
