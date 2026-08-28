package h8io.cfg.impl.yaml

import h8io.cfg.*
import org.snakeyaml.engine.v2.nodes.SequenceNode

import scala.jdk.CollectionConverters.*

private[yaml] final case class SeqImpl[+I <: Id](id: I, underlying: SequenceNode) extends Node.ISeq[I] {
  override def apply(index: Id.Index): INode[Id.Index] =
    if (index.fits(size)) wrap(index, underlying.getValue.get(index.index))
    else Node.None(index, this)

  override def tag: Option[String] = tagOf(underlying)

  override def iterator: Iterator[Node.ISome[Id.Index]] =
    underlying.getValue.asScala.iterator.zipWithIndex.map { case (value, i) => wrap(Id.Index(i, id), value) }

  override def location: Location = LocationImpl(underlying)

  override def size: Int = underlying.getValue.size

  override def toString: String = render(underlying)
}
