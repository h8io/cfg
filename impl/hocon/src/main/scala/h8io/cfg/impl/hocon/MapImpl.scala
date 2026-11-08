package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.*

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl[+I <: Id](id: I, underlying: ConfigObject) extends Node.IMap[I] {
  override def apply(key: String): INode[Id.Key] =
    if (underlying.containsKey(key)) wrap(Id.Key(key, id), underlying.get(key))
    else Node.None(Id.Key(key, id))

  override def tags: Set[String] = Set.empty

  override def iterator: Iterator[Node.ISome[Id.Key]] =
    underlying.entrySet.iterator.asScala.map(e => wrap(Id.Key(e.getKey, id), e.getValue))

  override def location: Location = LocationImpl(underlying)

  override def size: Int = underlying.size()

  override def -(key: String): MapImpl[I] =
    if (underlying.containsKey(key)) MapImpl(id, underlying.withoutKey(key)) else this

  override def toString: String = underlying.render(RenderOptions)
}
