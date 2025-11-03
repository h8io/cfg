package h8io.cfg

sealed trait CfgValue {
  def origin: CfgOrigin
}

final case class CfgNone(origin: CfgOrigin) extends CfgValue

final case class CfgNull(origin: CfgOrigin) extends CfgValue

final case class CfgScalar(value: String, origin: CfgOrigin) extends CfgValue

trait CfgMap extends CfgValue with Iterable[(String, CfgValue)] {
  def get(key: String): CfgValue
}

trait CfgList extends CfgValue with Iterable[CfgValue]
