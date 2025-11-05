package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.raw.{Id, Node, Origin}

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl[I <: Id](id: I, underlying: ConfigObject) extends Node.Map[I] {
  def apply(key: Id.Key): Node[Id.Key] =
    if (underlying.containsKey(key.key)) Wrap(key, underlying.get(key.key))
    else Node.None(key, OriginImpl(underlying.origin))

  def iterator: Iterator[Node[Id.Key]] =
    underlying.entrySet.iterator.asScala.map(e => Wrap(Id.Key(e.getKey, id), e.getValue))

  def origin: Origin = OriginImpl(underlying.origin)

  override def isEmpty: Boolean = underlying.isEmpty

  override def size: Int = underlying.size()

  override def knownSize: Int = underlying.size()
}
