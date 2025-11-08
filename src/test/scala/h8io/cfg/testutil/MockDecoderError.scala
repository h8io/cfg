package h8io.cfg.testutil

import h8io.cfg.DecoderError
import h8io.cfg.raw.{Id, Origin}

final case class MockDecoderError(id: Id, origin: Origin) extends DecoderError
