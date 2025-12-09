package h8io.cfg.raw

import h8io.cfg.NodeError

sealed trait Tag

object Tag {
  case object None extends Tag
  final case class Some(tag: String) extends Tag
  trait TagError extends Tag with NodeError
}
