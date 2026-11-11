package h8io.cfg.impl.psi

import scala.collection.mutable

/** Merges the syntax trees of the sources into one tree, in order (`format.md` §7): deep merge, the last one wins.
  *
  * Duplicate keys in a block, dotted keys and the sources passed to the loader are one rule: every field is merged in
  * turn into the map built so far, a block into the map at its key, so a `:=` anywhere, in a later source included,
  * replaces whatever its key held before. `a.b.c: v` is `a { b { c: v } }`, each block standing at its key.
  *
  * Directives are expanded before merging (§8): one left in a syntax tree is a bug of the loader.
  */
private[psi] final class Merge {
  private val errors = mutable.ListBuffer.empty[PsiError]

  /** The sources in order, each with where its root stands. */
  def sources(documents: ::[(Ast.Document, SourceLocation)]): Result[Tree.Map] = {
    val (first, site) = documents.head
    val root = documents.tail.foldLeft(block(Tree.Map.empty(Set.empty, site), first.statements)) {
      case (root, (document, site)) =>
        block(root.copy(sites = ::(site, root.sites)), document.statements)
    }
    Result(root, errors.toList)
  }

  /** Merges the statements of a block into `map`, in order. */
  private def block(map: Tree.Map, statements: Vector[Ast.Statement]): Tree.Map =
    statements.foldLeft(map) {
      case (map, Ast.Field(keys, operator, value, _)) => field(map, keys, operator, value)
      case (_, Ast.DirectiveStatement(call, _)) => unexpanded(call)
    }

  /** `keys: value`, the operator applying to the last key, every key before it a block at its key (§4). */
  private def field(map: Tree.Map, keys: ::[Ast.Key], operator: Ast.Operator, value: Ast.Value): Tree.Map =
    keys.tail match {
      case Nil => put(map, keys.head.name, operator, value)
      case rest @ ::(_, _) =>
        val nested = Ast.Field(rest, operator, value, None)
        put(map, keys.head.name, Ast.Operator.Merge, Ast.Block(Vector(nested), Set.empty, keys.head.location))
    }

  private def put(map: Tree.Map, key: String, operator: Ast.Operator, value: Ast.Value): Tree.Map =
    (operator, map.entries.get(key)) match {
      case (Ast.Operator.Merge, Some(earlier)) =>
        merged(earlier, value) match {
          case Some(tree) => map.updated(key, tree)
          case None => map.removed(key)
        }
      case _ => map.updated(key, tree(value))
    }

  /** `value` merged with a `:` over `earlier`: maps merge, their tags united, and anything else replaces what was
    * there, with only its own tags; none when either is a substitution and the other a map or a substitution.
    */
  private def merged(earlier: Tree.Value, value: Ast.Value): Option[Tree.Value] =
    (earlier, value) match {
      case (map: Tree.Map, Ast.Block(statements, tags, location)) =>
        Some(block(map.copy(tags = map.tags ++ tags, sites = ::(location, map.sites)), statements))
      case (_: Tree.Map | _: Tree.Substitution, substitution: Ast.Substitution) =>
        conflict(substitution.location, earlier)
      case (_: Tree.Substitution, block: Ast.Block) => conflict(block.location, earlier)
      case _ => Some(tree(value))
    }

  private def conflict(location: SourceLocation, earlier: Tree.Value): Option[Tree.Value] = {
    errors += MergeWithSubstitution(location, earlier.location)
    None
  }

  /** A value with nothing before it to merge with. */
  private def tree(value: Ast.Value): Tree.Value =
    value match {
      case Ast.Scalar(text, tags, location) => Tree.Scalar(text, tags, location)
      case Ast.Null(tags, location) => Tree.Null(tags, location)
      case Ast.Block(statements, tags, location) => block(Tree.Map.empty(tags, location), statements)
      case Ast.Sequence(elements, tags, location) => Tree.Sequence(elements.map(e => tree(e.value)), tags, location)
      case Ast.Substitution(path, optional, tags, location) => Tree.Substitution(path, optional, tags, location)
      case Ast.Concatenation(parts, tags, location) => Tree.Concatenation(parts, tags, location)
      case Ast.DirectiveValue(call, _) => unexpanded(call)
    }

  private def unexpanded(call: Ast.Call): Nothing =
    throw new IllegalStateException("#" + call.name + " at " + call.location.description + " was never expanded")
}

private[psi] object Merge {
  def apply(documents: ::[(Ast.Document, SourceLocation)]): Result[Tree.Map] = new Merge().sources(documents)
}
