package h8io.cfg.testutil

import h8io.cfg.DecoderError
import h8io.cfg.raw.Node

final case class MockDecoderError(node: Node) extends DecoderError
