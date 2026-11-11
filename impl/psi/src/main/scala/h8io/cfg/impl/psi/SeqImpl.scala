package h8io.cfg.impl.psi

import h8io.cfg.*

/** A sequence of a loaded configuration: a view over its merged tree. */
private[psi] final case class SeqImpl[+I <: Id](id: I, tree: Tree.Sequence) extends Node.ISeq[I] {
  override def apply(index: Int): INode[Id.Index] = {
    val childId = Id.Index(index, id)
    if (childId.fits(size)) wrap(childId, tree.elements(index)) else Node.None(childId)
  }

  override def tags: Set[String] = tree.tags

  override def iterator: Iterator[Node.ISome[Id.Index]] =
    tree.elements.iterator.zipWithIndex.map { case (value, i) => wrap(Id.Index(i, id), value) }

  override def location: Location = tree.location

  override def size: Int = tree.elements.size

  /** The sequence as Psi source, as a value. */
  override def toString: String = Render.value(tree)
}
