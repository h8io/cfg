package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.{CfgNone, CfgOrigin, CfgSeq, CfgValue}

import scala.jdk.CollectionConverters.*

final case class CfgSeqImpl(underlying: ConfigList) extends CfgSeq {
  def apply(i: Int): CfgValue =
    if (i < 0 || i >= underlying.size) CfgNone(CfgOriginImpl(underlying.origin)) else Wrap(underlying.get(i))

  def iterator: Iterator[CfgValue] = underlying.iterator.asScala.map(Wrap.apply)

  override def isEmpty: Boolean = underlying.isEmpty

  override def size: Int = underlying.size()

  override def knownSize: Int = underlying.size()

  def origin: CfgOrigin = CfgOriginImpl(underlying.origin)
}
