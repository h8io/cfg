package h8io.cfg.impl.psi

import h8io.cfg.*

/** A map of a loaded configuration: a view over its merged tree, its keys in the order they first appeared.
  *
  * @param location
  *   the tree's own, but at the root of a load given no source
  */
private[psi] final case class MapImpl[+I <: Id](id: I, tree: Tree.Map, location: PsiLocation) extends Node.IMap[I] {
  override def apply(key: String): INode[Id.Key] = {
    val childId = Id.Key(key, id)
    tree.entries.get(key).fold[INode[Id.Key]](Node.None(childId))(wrap(childId, _))
  }

  override def tags: Set[String] = tree.tags

  override def iterator: Iterator[Node.ISome[Id.Key]] =
    tree.keys.iterator.map(key => wrap(Id.Key(key, id), tree.entries(key)))

  override def size: Int = tree.keys.size

  override def -(key: String): MapImpl[I] = if (tree.entries.contains(key)) copy(tree = tree.removed(key)) else this
}
