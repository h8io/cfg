package h8io.cfg

import h8io.cfg.raw.Node

trait DecoderError extends CfgError {
  val node: Node.Some
}
