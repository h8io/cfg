package h8io.cfg

import h8io.cfg.raw.Node

trait Property[T] extends Decoder[Node.Map[?], T] {
  def name: String
}
