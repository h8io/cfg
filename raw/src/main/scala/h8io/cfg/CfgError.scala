package h8io.cfg

import h8io.cfg.raw.{Id, Node}

trait CfgError {
  def node: Node[Id]
}
