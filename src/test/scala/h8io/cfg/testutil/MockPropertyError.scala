package h8io.cfg.testutil

import h8io.cfg.CfgError
import h8io.cfg.raw.Node

final case class MockPropertyError(node: Node.Map) extends CfgError[Node.Map]
