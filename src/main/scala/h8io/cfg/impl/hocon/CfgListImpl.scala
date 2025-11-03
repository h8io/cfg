package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.{CfgList, CfgOrigin, CfgValue}

import scala.jdk.CollectionConverters.IteratorHasAsScala

final case class CfgListImpl(underlying: ConfigList) extends CfgList {
  override def iterator: Iterator[CfgValue] = underlying.iterator.asScala.map(wrap)

  override def origin: CfgOrigin = CfgOriginImpl(underlying.origin)
}
