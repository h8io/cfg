package h8io.cfg.testutil

import h8io.cfg.Decoder
import h8io.cfg.raw.Node

final case class MockDecoderError(node: Node.Value) extends Decoder.Error
