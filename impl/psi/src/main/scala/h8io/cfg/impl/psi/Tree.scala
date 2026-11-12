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
      extends Value

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
    * @param failed
    *   the keys left out because their merge failed (`format.md` §7): what refers to one fails silently (§11)
    */
  final case class Map(
      keys: Vector[String],
      entries: immutable.Map[String, Value],
      tags: Set[String],
      location: PsiLocation,
      failed: Set[String] = Set.empty)
      extends Value {

    /** This map with `value` at `key`: in its place when the key is there, last when it is not. */
    def updated(key: String, value: Value): Map =
      copy(
        keys = if (entries.contains(key)) keys else keys :+ key,
        entries = entries.updated(key, value),
        failed = failed - key)

    /** This map without `key`, its merge having failed. */
    def failedAt(key: String): Map = removed(key).copy(failed = failed + key)

    /** This map without `key`. */
    def removed(key: String): Map = copy(keys = keys.filter(_ != key), entries = entries - key)
  }

  object Map {
    def empty(tags: Set[String], location: PsiLocation): Map = Map(Vector.empty, immutable.Map.empty, tags, location)
  }
}
