package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigList
import h8io.cfg.*

import scala.jdk.CollectionConverters.*

private[hocon] final case class SeqImpl[+I <: Id](id: I, underlying: ConfigList) extends Node.ISeq[I] {
  override def apply(index: Int): INode[Id.Index] = {
    val childId = Id.Index(index, id)
    if (childId.fits(underlying.size)) wrap(childId, underlying.get(index)) else Node.None(childId, this)
  }

  override def tags: Set[String] = Set.empty

  override def iterator: Iterator[Node.ISome[Id.Index]] =
    underlying.iterator.asScala.zipWithIndex.map { case (value, i) => wrap(Id.Index(i, id), value) }

  override def location: Location = LocationImpl(underlying)

  override def size: Int = underlying.size()

  override def toString(): String = underlying.render(RenderOptions)
}
