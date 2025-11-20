package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.raw.{Id, Location, Node}

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl(id: Id, underlying: ConfigObject) extends Node.Map {
  def apply(key: Id.Key): Node =
    if (underlying.containsKey(key.key)) wrap(key, underlying.get(key.key))
    else Node.None(key, this)

  def iterator: Iterator[Node.Some] =
    underlying.entrySet.iterator.asScala.map(e => wrap(Id.Key(e.getKey, id), e.getValue))

  def location: Location = LocationImpl(underlying.origin)

  override def size: Int = underlying.size()

  override def toString: String = underlying.render(RenderOptions)
}
