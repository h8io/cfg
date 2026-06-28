package h8io.cfg.schema.errors

import h8io.cfg.{Node, NodeError}

sealed trait TagError extends NodeError

final case class NonScalarTag(node: Node.Map) extends TagError
final case class AmbiguousMap(node: Node.Map) extends TagError
