package h8io.cfg.impl

import h8io.cfg.{Id, Node}

package object psi {

  /** The node at `id` for a value of a merged tree, its substitutions all resolved. */
  private[psi] def wrap[I <: Id](id: I, value: Tree.Value): Node.ISome[I] =
    value match {
      case Tree.Scalar(text, tags, location) => Node.IScalar(id, tags, text, location)
      case Tree.Null(tags, location) => Node.INull(id, tags, location)
      case map: Tree.Map => MapImpl(id, map)
      case sequence: Tree.Sequence => SeqImpl(id, sequence)
      case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Marked =>
        throw new IllegalStateException("An unresolved substitution at " + value.location.description)
    }
}
