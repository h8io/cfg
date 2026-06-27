package h8io.cfg.fluent.errors

import h8io.cfg.{Node, NodeError}

final case class DummyNodeError(node: Node) extends NodeError
