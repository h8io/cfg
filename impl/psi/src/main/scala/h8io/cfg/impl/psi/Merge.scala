package h8io.cfg.impl.psi

/** Merges the syntax trees of the sources into one tree, in order (`format.md` §7): deep merge, the last one wins.
  *
  * Duplicate keys in a block, dotted keys and the sources passed to the loader are one rule: every field is merged in
  * turn into the map built so far, a block into the map at its key, so a `:=` anywhere, in a later source included,
  * replaces whatever its key held before. `a.b.c: v` is `a { b { c: v } }`, each block standing at its key.
  *
  * Directives are expanded before merging (§8): one left in a syntax tree is a bug of the loader. Merging finds no
  * error of its own: where it cannot tell maps from anything else yet, it keeps what it merges for resolution.
  */
private[psi] final class Merge {

  /** The sources in order, each with where its root stands. */
  def sources(documents: ::[(AST.Document, SourceLocation)]): Result[Tree.Map] = {
    val (first, site) = documents.head
    val root = documents.tail.foldLeft(block(Tree.Map.empty(Set.empty, site), first.statements)) {
      case (root, (document, site)) =>
        block(root.copy(location = Merge.again(site, root.location)), document.statements)
    }
    Result(root, Nil)
  }

  /** Merges the statements of a block into `map`, in order: from an injection on, they are kept as its steps, merged
    * once the injection is resolved (§9).
    */
  private[psi] def block(map: Tree.Map, statements: Vector[AST.Statement]): Tree.Map =
    statements.foldLeft(map) {
      case (map, statement) if map.steps.nonEmpty => map.copy(steps = Merge.step(map.steps, statement))
      case (map, AST.Field(keys, operator, value, _)) => field(map, keys, operator, value)
      case (map, AST.InjectionStatement(path, optional, location, _)) =>
        map.copy(steps = Vector(Tree.Step.Injection(path, optional, location)))
      case (_, AST.DirectiveStatement(call, _)) => unexpanded(call)
    }

  /** `keys: value`, the operator applying to the last key, every key before it a block at its key (§4). */
  private def field(map: Tree.Map, keys: ::[AST.Key], operator: AST.Operator, value: AST.Value): Tree.Map =
    keys.tail match {
      case Nil => put(map, keys.head.name, operator, value)
      case rest @ ::(_, _) =>
        val nested = AST.Field(rest, operator, value, None)
        put(map, keys.head.name, AST.Operator.Merge, AST.Block(Vector(nested), Set.empty, keys.head.location))
    }

  private def put(map: Tree.Map, key: String, operator: AST.Operator, value: AST.Value): Tree.Map =
    (operator, map.entries.get(key)) match {
      case (AST.Operator.Merge, Some(earlier)) => map.updated(key, merged(earlier, value))
      case _ => map.updated(key, tree(value))
    }

  /** `value` merged with a `:` over `earlier`: maps merge, their tags united, and anything else replaces what was
    * there, with only its own tags. Where either may turn out a map or not, a substitution or a block of injections
    * alone, and the other may be a map, both are kept as layers, merged so once resolved (§7).
    */
  private def merged(earlier: Tree.Value, value: AST.Value): Tree.Value =
    (earlier, value) match {
      case (map: Tree.Map, AST.Block(statements, tags, location)) =>
        block(map.copy(tags = map.tags ++ tags, location = Merge.again(location, map.location)), statements)
      case (earlier: Tree.Layer, AST.Substitution(path, optional, tags, location)) =>
        Tree.Layers(earlier, Tree.Substitution(path, optional, tags, location))
      case (earlier: Tree.Layer, injections: AST.Injections) => Tree.Layers(earlier, this.injections(injections))
      case (Merge.Unknown(earlier), AST.Block(statements, tags, location)) =>
        Tree.Layers(earlier, block(Tree.Map.empty(tags, location), statements))
      case _ => tree(value)
    }

  /** A value with nothing before it to merge with. */
  private def tree(value: AST.Value): Tree.Value =
    value match {
      case AST.Scalar(text, tags, location) => Tree.Scalar(text, tags, location)
      case AST.Null(tags, location) => Tree.Null(tags, location)
      case AST.Block(statements, tags, location) => block(Tree.Map.empty(tags, location), statements)
      case AST.Sequence(elements, tags, location) => sequence(elements, tags, location)
      case AST.Substitution(path, optional, tags, location) => Tree.Substitution(path, optional, tags, location)
      case injections: AST.Injections => this.injections(injections)
      case AST.Concatenation(parts, tags, location) => Tree.Concatenation(parts, tags, location)
      case AST.Marked(value, markers) => Tree.Marked(tree(value), markers)
      case AST.DirectiveValue(call, _) => unexpanded(call)
    }

  private def injections(injections: AST.Injections): Tree.Injections = {
    val kept = injections.items.map {
      case AST.Injected.Calling(call) => unexpanded(call)
      case gave: AST.Injected.Given => Left(gave)
      case AST.Injected.Referring(reference) => Right(reference)
    }
    Tree.Injections(kept, injections.tags, injections.location)
  }

  /** A sequence of `elements`, with nothing before it to merge with. */
  private[psi] def sequence(elements: Vector[AST.Element], tags: Set[String], location: PsiLocation): Tree.Sequence =
    Tree.Sequence(elements.map(element), tags, location)

  /** An element of a sequence, where a substitution may be an injection (§9). */
  private def element(element: AST.Element): Tree.Value =
    (element.value, element.put) match {
      case (AST.Substitution(path, optional, tags, location), AST.Put.Found) =>
        Tree.Injection(path, optional, tags, location, required = false)
      case (AST.Substitution(path, optional, tags, location), AST.Put.Elements) =>
        Tree.Injection(path, optional, tags, location, required = true)
      case (value, _) => tree(value)
    }

  private def unexpanded(call: AST.Call): Nothing =
    throw new IllegalStateException("#" + call.name + " at " + call.location.description + " was never expanded")
}

private[psi] object Merge {

  /** A value that may turn out a map or not, known only once resolved. */
  private object Unknown {
    def unapply(value: Tree.Value): Option[Tree.Layer] =
      value match {
        case _: Tree.Map => None
        case layer: Tree.Layer => Some(layer)
        case _ => None
      }
  }

  /** `steps` with `statement` after them: an injection a step of its own, any other statement among those after the
    * last.
    */
  private def step(steps: Vector[Tree.Step], statement: AST.Statement): Vector[Tree.Step] =
    (statement, steps.last) match {
      case (AST.InjectionStatement(path, optional, location, _), _) =>
        steps :+ Tree.Step.Injection(path, optional, location)
      case (_, Tree.Step.Statements(statements)) => steps.init :+ Tree.Step.Statements(statements :+ statement)
      case _ => steps :+ Tree.Step.Statements(Vector(statement))
    }

  /** The location of a map written at `earlier` and again at `site`. */
  private[psi] def again(site: PsiLocation, earlier: PsiLocation): MergedLocation =
    earlier match {
      case MergedLocation(at, rest) => MergedLocation(site, ::(at, rest))
      case at => MergedLocation(site, ::(at, Nil))
    }

  def apply(documents: ::[(AST.Document, SourceLocation)]): Result[Tree.Map] = new Merge().sources(documents)
}
