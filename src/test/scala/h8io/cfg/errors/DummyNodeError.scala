package h8io.cfg.errors

import h8io.cfg.NodeError
import h8io.cfg.raw.Node

final case class DummyNodeError(node: Node) extends NodeError
