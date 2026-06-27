package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.*

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl[+I <: Id](id: I, underlying: ConfigObject) extends Node.IMap[I] {
  override def apply(key: Id.Key): INode[Id.Key] =
    if (underlying.containsKey(key.key)) wrap(key, underlying.get(key.key))
    else Node.None(key, this)

  override def tag: Option[String] = None

  override def iterator: Iterator[Node.ISome[Id.Key]] =
    underlying.entrySet.iterator.asScala.map(e => wrap(Id.Key(e.getKey, id), e.getValue))

  override def location: Location = LocationImpl(underlying)

  override def size: Int = underlying.size()

  override def -(key: String): MapImpl[I] =
    if (underlying.containsKey(key)) MapImpl(id, underlying.withoutKey(key)) else this

  override def toString: String = underlying.render(RenderOptions)
}
