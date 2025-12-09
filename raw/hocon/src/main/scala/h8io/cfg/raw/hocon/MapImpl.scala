package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.raw.{INode, Id, Location, Node}

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl[+I <: Id](id: I, underlying: ConfigObject, tag: Option[String])
    extends Node.IMap[I] {
  def apply(key: Id.Key): INode[Id.Key] =
    if (underlying.containsKey(key.key)) wrap(key, underlying.get(key.key))
    else Node.None(key, this)

  def iterator: Iterator[Node.ISome[Id.Key]] =
    underlying.entrySet.iterator.asScala.map(e => wrap(Id.Key(e.getKey, id), e.getValue))

  def location: Location = LocationImpl(underlying.origin)

  override def size: Int = underlying.size()

  override def toString: String = underlying.render(RenderOptions)
}
