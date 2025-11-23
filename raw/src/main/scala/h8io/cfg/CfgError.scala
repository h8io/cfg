package h8io.cfg

import h8io.cfg.raw.Node

trait CfgError[+N <: Node] {
  def node: N
}

object CfgError {
  type Any = CfgError[Node]
}
