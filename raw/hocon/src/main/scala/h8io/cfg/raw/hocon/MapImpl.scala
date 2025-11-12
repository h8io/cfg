package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.raw.{Id, Node, Origin}

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl(id: Id, underlying: ConfigObject) extends Node.Map {
  def apply(key: Id.Key): Node =
    if (underlying.containsKey(key.key)) wrap(key, underlying.get(key.key))
    else Node.None(key, OriginImpl(underlying.origin))

  def iterator: Iterator[Node.Some] =
    underlying.entrySet.iterator.asScala.map(e => wrap(Id.Key(e.getKey, id), e.getValue))

  def origin: Origin = OriginImpl(underlying.origin)

  override def isEmpty: Boolean = underlying.isEmpty

  override def size: Int = underlying.size()

  override def knownSize: Int = underlying.size()
}
