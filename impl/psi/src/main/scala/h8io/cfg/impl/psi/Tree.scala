package h8io.cfg.impl.psi

import h8io.cfg.Id

import scala.collection.immutable

/** A configuration tree merged from one source or several (`format.md` §7): no operators, no duplicate keys, no
  * directives. Merged, its substitutions are still unresolved; resolved (§9), it holds none, and a grafted node carries
  * a [[ReferenceLocation]].
  */
private[psi] object Tree {

  sealed trait Value {
    def tags: Set[String]
    def location: PsiLocation
  }

  final case class Scalar(value: String, tags: Set[String], location: PsiLocation) extends Value

  final case class Null(tags: Set[String], location: PsiLocation) extends Value

  final case class Sequence(elements: Vector[Value], tags: Set[String], location: PsiLocation) extends Value

  /** `\${path}` or `\${?path}`, alone and unquoted: a graft. */
  final case class Substitution(path: Id.Branch, optional: Boolean, tags: Set[String], location: SourceLocation)
      extends Layer

  /** `\${path}` or `\${?path}` among the elements of a sequence, an injection: the elements of the sequence it finds,
    * in its place (§9). What a `required` one finds must be a sequence; any other puts anything else as one element,
    * with its tags.
    */
  final case class Injection(
      path: Id.Branch,
      optional: Boolean,
      tags: Set[String],
      location: SourceLocation,
      required: Boolean)
      extends Value

  /** A block of substitutions and what calls gave alone, two or more (§4, §9): the fields of the maps they find or give
    * merged in turn, or the elements of the sequences, as the first that finds or gives anything says; nothing when
    * none does.
    *
    * @param items
    *   in the order written, what a call gave or a reference
    */
  final case class Injections(
      items: Vector[Either[AST.Injected.Given, Token.Reference]],
      tags: Set[String],
      location: SourceLocation)
      extends Layer

  /** A value `:` merges as a layer (`format.md` §7): a map, or what may turn out a map or not once resolved. */
  sealed trait Layer extends Value

  /** What `:` merged in turn at one key, where a substitution or a block of injections alone stands among them, so that
    * whether the merge is of maps is known only once they are resolved (`format.md` §7): maps merge, anything else
    * replaces what is before it. Each layer is a map, a substitution, a block of injections alone, or layers merged in
    * their turn into a map.
    */
  final case class Layers(layers: Vector[Layer]) extends Layer {

    /** What the layers end up as says its tags. */
    def tags: Set[String] = Set.empty

    def location: PsiLocation = layers.last.location
  }

  object Layers {

    /** `value` merged with `:` over `earlier`, both kept for resolution. */
    def apply(earlier: Layer, value: Layer): Layers =
      earlier match {
        case Layers(layers) => Layers(layers :+ value)
        case _ => Layers(Vector(earlier, value))
      }
  }

  /** A scalar with `\${…}` inside it. */
  final case class Concatenation(parts: Vector[Token.Part], tags: Set[String], location: SourceLocation) extends Value

  /** A block with the loader's markers to run over its value, a scalar or a concatenation, once resolved (§5). */
  final case class Marked(value: Value, markers: Vector[MarkerParser.MarkerCall]) extends Value {
    def tags: Set[String] = value.tags
    def location: PsiLocation = value.location
  }

  /** A map, its keys in the order they first appeared (`format.md` §7).
    *
    * @param location
    *   where it was defined: one place, or a [[MergedLocation]] of each block merged into it since it was last replaced
    * @param steps
    *   what was written in the map from its first injection on, to be merged into it in order once each injection is
    *   resolved (§9); the keys and entries are the fields written before it. A map with none is complete.
    */
  final case class Map(
      keys: Vector[String],
      entries: immutable.Map[String, Value],
      tags: Set[String],
      location: PsiLocation,
      steps: Vector[Step] = Vector.empty)
      extends Layer {

    /** This map with `value` at `key`: in its place when the key is there, last when it is not. */
    def updated(key: String, value: Value): Map =
      copy(keys = if (entries.contains(key)) keys else keys :+ key, entries = entries.updated(key, value))

    /** This map without `key`. */
    def removed(key: String): Map = copy(keys = keys.filter(_ != key), entries = entries - key)
  }

  object Map {
    def empty(tags: Set[String], location: PsiLocation): Map = Map(Vector.empty, immutable.Map.empty, tags, location)
  }

  /** What is merged into a map after its first injection, in order (§9). */
  sealed trait Step

  object Step {

    /** An injection, `$path` among fields, of the map at `path`. */
    final case class Injection(path: Id.Branch, optional: Boolean, location: SourceLocation) extends Step

    /** Statements written after an injection, merged as they would have been. */
    final case class Statements(statements: Vector[AST.Statement]) extends Step

    /** A map an injection around this one brought in, already resolved, merged as an injection's is. */
    final case class Graft(map: Map) extends Step
  }
}
