package h8io.cfg.testutil

import h8io.cfg.NodeError
import h8io.cfg.raw.Node

final case class MockNodeError(node: Node) extends NodeError
