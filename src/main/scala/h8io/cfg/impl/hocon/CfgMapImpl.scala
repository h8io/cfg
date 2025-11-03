package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.{CfgMap, CfgNone, CfgOrigin, CfgValue}

import scala.jdk.CollectionConverters.IteratorHasAsScala

final case class CfgMapImpl(underlying: ConfigObject) extends CfgMap {
  def get(key: String): CfgValue =
    if (underlying.containsKey(key)) wrap(underlying.get(key)) else CfgNone(CfgOriginImpl(underlying.origin))

  def iterator: Iterator[(String, CfgValue)] =
    underlying.entrySet.iterator.asScala.map(e => e.getKey -> wrap(e.getValue))

  def origin: CfgOrigin = CfgOriginImpl(underlying.origin)
}
