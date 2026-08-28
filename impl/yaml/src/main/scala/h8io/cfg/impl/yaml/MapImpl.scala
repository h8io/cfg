package h8io.cfg.impl.yaml

import h8io.cfg.*
import org.snakeyaml.engine.v2.nodes.{MappingNode, NodeTuple}

import scala.jdk.CollectionConverters.*

private[yaml] final case class MapImpl[+I <: Id](id: I, underlying: MappingNode) extends Node.IMap[I] {
  private lazy val entries: Vector[(String, NodeTuple)] = entriesOf(underlying)

  private lazy val index: Map[String, NodeTuple] = entries.toMap

  override def apply(key: Id.Key): INode[Id.Key] =
    index.get(key.key).fold[INode[Id.Key]](Node.None(key, this))(tuple => wrap(key, tuple.getValueNode))

  override def tag: Option[String] = tagOf(underlying)

  override def iterator: Iterator[Node.ISome[Id.Key]] =
    entries.iterator.map { case (key, tuple) => wrap(Id.Key(key, id), tuple.getValueNode) }

  override def location: Location = LocationImpl(underlying)

  override def size: Int = entries.size

  override def -(key: String): MapImpl[I] =
    if (index.contains(key)) MapImpl(id, copyOf(underlying, entries.filterNot(_._1 == key).map(_._2).asJava))
    else this

  /** Renders the mapping as seen through this node, i.e. with duplicate keys already collapsed. */
  override def toString: String = render(copyOf(underlying, entries.map(_._2).asJava))
}
