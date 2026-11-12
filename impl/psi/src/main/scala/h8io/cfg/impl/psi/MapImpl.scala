package h8io.cfg.impl.psi

import h8io.cfg.*

/** A map of a loaded configuration: a view over its resolved tree, its keys in the order they first appeared. */
private[psi] final case class MapImpl[+I <: Id](id: I, tree: Tree.Map) extends Node.IMap[I] {
  override def apply(key: String): INode[Id.Key] = {
    val childId = Id.Key(key, id)
    tree.entries.get(key).fold[INode[Id.Key]](Node.None(childId))(wrap(childId, _))
  }

  override def tags: Set[String] = tree.tags

  override def location: Location = tree.location

  override def iterator: Iterator[Node.ISome[Id.Key]] =
    tree.keys.iterator.map(key => wrap(Id.Key(key, id), tree.entries(key)))

  override def size: Int = tree.keys.size

  override def -(key: String): MapImpl[I] = if (tree.entries.contains(key)) copy(tree = tree.removed(key)) else this

  /** The map as Psi source: the root as a document, its fields one per line, any other map as a value. */
  override def toString: String = if (id == Id.Root) Render.document(tree) else Render.value(tree)
}
