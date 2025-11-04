package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.raw.{Entry, Origin, Ref}

import scala.jdk.CollectionConverters.*

private[hocon] final case class SeqImpl[P <: Ref](id: P, underlying: ConfigList) extends Entry.Seq[P] {
  def apply(index: Ref.Index): Entry[Ref.Index] =
    if (index.fits(underlying.size)) Wrap(index, underlying.get(index.index))
    else Entry.None(index, OriginImpl(underlying.origin))

  def iterator: Iterator[Entry[Ref.Index]] =
    underlying.iterator.asScala.zipWithIndex.map { case (value, i) => Wrap(Ref.Index(i), value) }

  def origin: Origin = OriginImpl(underlying.origin)

  override def isEmpty: Boolean = underlying.isEmpty

  override def size: Int = underlying.size()

  override def knownSize: Int = underlying.size()
}
