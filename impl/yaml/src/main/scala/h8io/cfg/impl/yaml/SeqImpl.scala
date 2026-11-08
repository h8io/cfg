package h8io.cfg.impl.yaml

import h8io.cfg.*
import org.snakeyaml.engine.v2.nodes.SequenceNode

import scala.jdk.CollectionConverters.*

private[yaml] final case class SeqImpl[+I <: Id](id: I, underlying: SequenceNode) extends Node.ISeq[I] {
  override def apply(index: Int): INode[Id.Index] = {
    val childId = Id.Index(index, id)
    if (childId.fits(size)) wrap(childId, underlying.getValue.get(index)) else Node.None(childId, this)
  }

  override def tags: Set[String] = tagOf(underlying)

  override def iterator: Iterator[Node.ISome[Id.Index]] =
    underlying.getValue.asScala.iterator.zipWithIndex.map { case (value, i) => wrap(Id.Index(i, id), value) }

  override def location: Location = LocationImpl(underlying)

  override def size: Int = underlying.getValue.size

  override def toString: String = render(underlying)
}
