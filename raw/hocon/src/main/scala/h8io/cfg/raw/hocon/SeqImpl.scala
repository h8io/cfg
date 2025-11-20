package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.raw.{Id, Location, Node}

import scala.jdk.CollectionConverters.*

private[hocon] final case class SeqImpl(id: Id, underlying: ConfigList) extends Node.Seq {
  def apply(index: Id.Index): Node =
    if (index.fits(underlying.size)) wrap(index, underlying.get(index.index))
    else Node.None(index, this)

  def iterator: Iterator[Node.Some] =
    underlying.iterator.asScala.zipWithIndex.map { case (value, i) => wrap(Id.Index(i, id), value) }

  def location: Location = LocationImpl(underlying.origin)

  override def size: Int = underlying.size()

  override def toString(): String = underlying.render(RenderOptions)
}
