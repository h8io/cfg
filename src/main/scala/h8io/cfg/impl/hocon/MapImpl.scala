package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigObject
import h8io.cfg.{Entry, Origin, Path}

import scala.jdk.CollectionConverters.*

private[hocon] final case class MapImpl[P <: Path](path: P, underlying: ConfigObject) extends Entry.Map[P] {
  def apply(key: Path.Key): Entry[Path.Key] =
    if (underlying.containsKey(key.key)) Wrap(key, underlying.get(key.key))
    else Entry.None(key, OriginImpl(underlying.origin))

  def iterator: Iterator[Entry[Path.Key]] =
    underlying.entrySet.iterator.asScala.map(e => Wrap(Path.Key(e.getKey), e.getValue))

  def origin: Origin = OriginImpl(underlying.origin)

  override def isEmpty: Boolean = underlying.isEmpty

  override def size: Int = underlying.size()

  override def knownSize: Int = underlying.size()
}
