package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.raw.{Id, Location, Node}

import scala.jdk.CollectionConverters.*

private[hocon] final case class SeqImpl[+I <: Id](id: I, underlying: ConfigList) extends Node.Seq[I] {
  def apply(index: Id.Index): Node[Id.Index] =
    if (index.fits(underlying.size)) wrap(index, underlying.get(index.index))
    else Node.None(index, this)

  def iterator: Iterator[Node.Some[Id.Index]] =
    underlying.iterator.asScala.zipWithIndex.map { case (value, i) => wrap(Id.Index(i, id), value) }

  def location: Location = LocationImpl(underlying.origin)

  override def size: Int = underlying.size()

  override def toString(): String = underlying.render(RenderOptions)
}
