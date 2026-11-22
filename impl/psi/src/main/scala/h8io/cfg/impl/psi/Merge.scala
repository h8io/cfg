package h8io.cfg.impl.psi

/** Merges the syntax trees of the sources into one tree, in order: deep merge, the last one wins.
  *
  * Duplicate keys in a block, dotted keys and the sources passed to the loader are one rule: every field is merged in
  * turn into the map built so far, a block into the map at its key, so a `:=` anywhere, in a later source included,
  * replaces whatever its key held before. `a.b.c: v` is `a { b { c: v } }`, each block standing at its key.
  *
  * Directives are expanded before merging: one left in a syntax tree is a bug of the loader. Merging finds no error of
  * its own: where it cannot tell maps from anything else yet, it keeps what it merges for resolution.
  */
private[psi] final class Merge {

  /** The sources in order, each with where its root stands. */
  def sources(documents: ::[(AST.Document, SourceLocation)]): Result[Tree.Map] = {
    val (first, site) = documents.head
    val root = documents.tail.foldLeft(block(Tree.Map.empty(None, site), first.statements)) {
      case (root, (document, site)) =>
        block(root.copy(location = Merge.again(site, root.location)), document.statements)
    }
    Result(root, Nil)
  }

  /** Merges the statements of a block into `map`, in order: from an injection on, they are kept as its steps, merged
    * once the injection is resolved.
    */
  private[psi] def block(map: Tree.Map, statements: Vector[AST.Statement]): Tree.Map =
    statements.foldLeft(map) {
      case (map, statement) if map.steps.nonEmpty => map.copy(steps = Merge.step(map.steps, statement))
      case (map, AST.Field(keys, operator, value, _)) => field(map, keys, operator, value)
      case (map, AST.InjectionStatement(path, optional, location, _)) =>
        map.copy(steps = Vector(Tree.Step.Injection(path, optional, location)))
      case (_, AST.DirectiveStatement(call, _)) => unexpanded(call)
    }

  /** `keys: value`, the operator applying to the last key, every key before it a block at its key. */
  private def field(map: Tree.Map, keys: ::[AST.Key], operator: AST.Operator, value: AST.Value): Tree.Map =
    keys.tail match {
      case Nil => put(map, keys.head.name, operator, value)
      case rest @ ::(_, _) =>
        val nested = AST.Field(rest, operator, value, None)
        put(map, keys.head.name, AST.Operator.Merge, AST.Block(Vector(nested), None, keys.head.location))
    }

  private def put(map: Tree.Map, key: String, operator: AST.Operator, value: AST.Value): Tree.Map =
    (operator, map.entries.get(key)) match {
      case (AST.Operator.Merge, Some(earlier)) => map.updated(key, merged(earlier, value))
      case _ => map.updated(key, tree(value))
    }

  /** `value` merged with a `:` over `earlier`: maps merge, the later tag if written and the earlier otherwise, and
    * anything else replaces what was there, with only its own tag. Where either may turn out a map or not, a
    * substitution or an injection after a key, and the other may be a map, both are kept as layers, merged so once
    * resolved.
    */
  private def merged(earlier: Tree.Value, value: AST.Value): Tree.Value =
    (earlier, value) match {
      case (map: Tree.Map, AST.Block(statements, tag, location)) =>
        block(map.copy(tag = tag.orElse(map.tag), location = Merge.again(location, map.location)), statements)
      case (earlier: Tree.Layer, AST.Substitution(path, optional, tag, location)) =>
        Tree.Layers(earlier, Tree.Substitution(path, optional, tag, location))
      case (earlier: Tree.Layer, contents: AST.Contents) => Tree.Layers(earlier, this.contents(contents))
      case (earlier: Tree.Layer, AST.Formatted(part, tag, location)) =>
        Tree.Layers(earlier, Tree.Formatted(part, tag, location))
      // what finds nothing puts nothing, and what stands before it, no map, stands then
      case (earlier, AST.Substitution(path, true, tag, location)) =>
        Tree.Layers(Tree.Kept(earlier), Tree.Substitution(path, optional = true, tag, location))
      case (earlier, contents @ AST.Contents(AST.Injected.Referring(reference), _, _)) if reference.optional =>
        Tree.Layers(Tree.Kept(earlier), this.contents(contents))
      case (earlier, AST.Formatted(part, tag, location)) if part.reference.optional =>
        Tree.Layers(Tree.Kept(earlier), Tree.Formatted(part, tag, location))
      // kept as written, so that a `:=` in it replaces what the earlier finds, once it is found
      case (Merge.Unknown(earlier), AST.Block(statements, tag, location)) =>
        Tree.Layers(earlier, Tree.Map.empty(tag, location).copy(steps = Merge.steps(statements)))
      case _ => tree(value)
    }

  /** A value with nothing before it to merge with. */
  private def tree(value: AST.Value): Tree.Value =
    value match {
      case AST.Scalar(text, tag, location) => Tree.Scalar(text, tag, location)
      case AST.Null(tag, location) => Tree.Null(tag, location)
      case AST.Block(statements, tag, location) => block(Tree.Map.empty(tag, location), statements)
      case AST.Sequence(elements, tag, location) => sequence(elements, tag, location)
      case AST.Substitution(path, optional, tag, location) => Tree.Substitution(path, optional, tag, location)
      case contents: AST.Contents => this.contents(contents)
      case AST.Concatenation(parts, tag, location) => Tree.Concatenation(parts, tag, location)
      case AST.Formatted(part, tag, location) => Tree.Formatted(part, tag, location)
      case AST.Marked(value, markers) => Tree.Marked(tree(value), markers)
      case AST.DirectiveValue(call, _) => unexpanded(call)
    }

  private def contents(contents: AST.Contents): Tree.Contents = {
    val kept = contents.item match {
      case AST.Injected.Calling(call) => unexpanded(call)
      case gave: AST.Injected.Given => Left(gave)
      case AST.Injected.Referring(reference) => Right(reference)
    }
    Tree.Contents(kept, contents.tag, contents.location)
  }

  /** A sequence of `elements`, with nothing before it to merge with. */
  private[psi] def sequence(elements: Vector[AST.Element], tag: Option[String], location: PsiLocation): Tree.Sequence =
    Tree.Sequence(elements.map(element), tag, location)

  /** An element of a sequence, where a substitution may be an injection. */
  private def element(element: AST.Element): Tree.Value =
    (element.value, element.put) match {
      case (AST.Substitution(path, optional, _, location), AST.Put.Elements) => Tree.Injection(path, optional, location)
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

  /** `statements` kept as steps, merged in order once what they merge over is known. */
  private def steps(statements: Vector[AST.Statement]): Vector[Tree.Step] =
    statements.foldLeft(Vector.empty[Tree.Step]) {
      case (steps, statement) if steps.nonEmpty => step(steps, statement)
      case (_, AST.InjectionStatement(path, optional, location, _)) =>
        Vector(Tree.Step.Injection(path, optional, location))
      case (_, statement) => Vector(Tree.Step.Statements(Vector(statement)))
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
