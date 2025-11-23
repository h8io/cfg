package h8io.cfg.errors

import h8io.cfg.CfgError
import h8io.cfg.raw.Node

final case class Thrown[N <: Node](node: N, cause: Throwable) extends CfgError[N]
