package h8io.cfg

import h8io.cfg.raw.Node

trait NodeError extends CfgError {
  def node: Node
}
