package h8io.cfg.testutil

import h8io.cfg.CfgError
import h8io.cfg.raw.{Id, Node}

final case class MockCfgError(node: Node[Id]) extends CfgError
