package h8io.cfg

sealed trait CfgValue {
  def origin: CfgOrigin
}

final case class CfgNone(origin: CfgOrigin) extends CfgValue

final case class CfgNull(origin: CfgOrigin) extends CfgValue

final case class CfgScalar(value: String, origin: CfgOrigin) extends CfgValue

trait CfgMap extends CfgValue with Iterable[(String, CfgValue)] {
  def apply(key: String): CfgValue

  def isEmpty: Boolean

  def size: Int

  def knownSize: Int
}

trait CfgSeq extends CfgValue with Iterable[CfgValue] {
  def apply(i: Int): CfgValue

  def knownSize: Int
}
