package h8io.cfg.impl.psi

import h8io.cfg.Id

import scala.collection.immutable

/** A configuration tree merged from one source or several: no operators, no duplicate keys, no directives. Merged, its
  * substitutions are still unresolved; resolved, it holds none, and a grafted node carries a [[ReferenceLocation]].
  */
private[psi] object Tree {

  sealed trait Value {
    def tag: Option[String]
    def location: PsiLocation
  }

  final case class Scalar(value: String, tag: Option[String], location: PsiLocation) extends Value

  final case class Null(tag: Option[String], location: PsiLocation) extends Value

  final case class Sequence(elements: Vector[Value], tag: Option[String], location: PsiLocation) extends Value

  /** `\${path}` or `\${?path}`, alone and unquoted: a graft. */
  final case class Substitution(path: Id.Branch, optional: Boolean, tag: Option[String], location: SourceLocation)
      extends Layer

  /** `\${path*}` or `\${?path*}` among the elements of a sequence, an injection: the elements of the sequence it finds,
    * which it must be, in its place.
    */
  final case class Injection(path: Id.Branch, optional: Boolean, location: SourceLocation) extends Value {

    /** None: the elements it puts keep their own, and nothing written before it may tag them. */
    def tag: Option[String] = None
  }

  /** An injection after a key, `a: \${b*}` or `a: #d(…)*`: a map holding the fields of the map found or given, or a
    * sequence holding the elements of the sequence, with `tag` and none of the found node's own; nothing when an
    * optional one finds nothing.
    *
    * @param item
    *   what the call gave, or the reference
    */
  final case class Contents(
      item: Either[AST.Injected.Given, Token.Reference],
      tag: Option[String],
      location: SourceLocation)
      extends Layer

  /** A value `:` merges as a layer: a map, or what may turn out a map or not once resolved. */
  sealed trait Layer extends Value

  /** What `:` merged in turn at one key, where a substitution or an injection after a key stands among them, so that
    * whether the merge is of maps is known only once they are resolved: maps merge, anything else replaces what is
    * before it. Each layer is a map, a substitution, formatted or not, an injection after a key, layers merged in their
    * turn into a map, or first a value that is no map kept below an optional one ([[Kept]]).
    */
  final case class Layers(layers: Vector[Layer]) extends Layer {

    /** What the layers end up as says its tag. */
    def tag: Option[String] = None

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

  /** A value that is no map, below an optional substitution, formatted or not, or an injection after a key, merged over
    * it: what finds nothing puts nothing, and the value then stands. Only ever the first of [[Layers]].
    */
  final case class Kept(value: Value) extends Layer {
    def tag: Option[String] = value.tag
    def location: PsiLocation = value.location
  }

  /** A scalar with `\${…}` inside it. */
  final case class Concatenation(parts: Vector[Token.Part], tag: Option[String], location: SourceLocation) extends Value

  /** `\${path; name(…)}` unquoted and alone: a layer, since an optional one may give nothing, and what it merges over
    * then stands.
    */
  final case class Formatted(part: Token.Formatted, tag: Option[String], location: SourceLocation) extends Layer

  /** A block with the loader's markers to run over its value, a scalar or a concatenation, once resolved. */
  final case class Marked(value: Value, markers: Vector[MarkerParser.MarkerCall]) extends Value {
    def tag: Option[String] = value.tag
    def location: PsiLocation = value.location
  }

  /** A map, its keys in the order they first appeared.
    *
    * @param location
    *   where it was defined: one place, or a [[MergedLocation]] of each block merged into it since it was last replaced
    * @param steps
    *   what was written in the map from its first injection on, to be merged into it in order once each injection is
    *   resolved; the keys and entries are the fields written before it. A map with none is complete.
    */
  final case class Map(
      keys: Vector[String],
      entries: immutable.Map[String, Value],
      tag: Option[String],
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
    def empty(tag: Option[String], location: PsiLocation): Map = Map(Vector.empty, immutable.Map.empty, tag, location)
  }

  /** What is merged into a map after its first injection, in order. */
  sealed trait Step

  object Step {

    /** An injection, `$path*` among fields, of the map at `path`. */
    final case class Injection(path: Id.Branch, optional: Boolean, location: SourceLocation) extends Step

    /** Statements written after an injection, merged as they would have been. */
    final case class Statements(statements: Vector[AST.Statement]) extends Step

    /** A map an injection around this one brought in, already resolved, merged as an injection's is. */
    final case class Graft(map: Map) extends Step
  }
}
