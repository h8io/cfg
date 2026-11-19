package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

/** What a [[Directive]] gives back: Psi's own unexpanded tree, opaque to the handler. A fragment is a value: it carries
  * the errors found in making it, reported wherever it is used, so a handler may keep one and give it back again, in
  * the same load or a later one. [[Fragment.None]] is nothing at all, as `Node.None` is a node that is not there.
  */
sealed trait Fragment

object Fragment {

  /** Nothing: no field in statement position, and in value position the field or the element left out, as an optional
    * substitution that finds nothing leaves it out (§6). Not an empty map, which `a: #d` would give as `a: {}`.
    */
  case object None extends Fragment

  /** A fragment that holds something, as `Node.Some` is a node that is there. */
  private[psi] sealed trait Some extends Fragment

  /** A fragment of `node`, which holds no substitution; its ids are dropped, and its locations are kept as origins
    * behind the call (§10).
    */
  def apply(node: Node.ISome[Id]): Fragment = Built(node)

  /** A source parsed, its directives expanded; `root` is where it starts, `errors` what was found in it, reported
    * wherever the fragment is used, and `sources` the names of it and of the sources it parsed, in the order parsed.
    */
  private[psi] final case class Parsed(
      document: AST.Document,
      root: SourceLocation,
      errors: List[PsiError],
      sources: List[String])
      extends Some

  private[psi] final case class Built(node: Node.ISome[Id]) extends Some
}
