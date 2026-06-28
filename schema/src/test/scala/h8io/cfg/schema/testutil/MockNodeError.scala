package h8io.cfg.schema.testutil

import h8io.cfg.{Node, NodeError}

final case class MockNodeError(node: Node) extends NodeError
