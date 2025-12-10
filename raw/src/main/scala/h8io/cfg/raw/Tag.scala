package h8io.cfg.raw

import h8io.cfg.CfgError

sealed trait Tag {
  def location: Location
}

object Tag {
  final case class None(location: Location) extends Tag
  final case class Some(tag: String, location: Location) extends Tag
  trait Error extends Tag with CfgError
}
