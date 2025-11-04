package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.{Entry, Origin, Path}

import scala.jdk.CollectionConverters.*

private[hocon] final case class SeqImpl[P <: Path](path: P, underlying: ConfigList) extends Entry.Seq[P] {
  def apply(index: Path.Index): Entry[Path.Index] =
    if (index.fits(underlying.size)) Wrap(index, underlying.get(index.index))
    else Entry.None(index, OriginImpl(underlying.origin))

  def iterator: Iterator[Entry[Path.Index]] =
    underlying.iterator.asScala.zipWithIndex.map { case (value, i) => Wrap(Path.Index(i), value) }

  def origin: Origin = OriginImpl(underlying.origin)

  override def isEmpty: Boolean = underlying.isEmpty

  override def size: Int = underlying.size()

  override def knownSize: Int = underlying.size()
}
