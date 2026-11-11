package h8io.cfg.impl.psi

import h8io.cfg.Id

import scala.collection.immutable

/** A configuration tree merged from one source or several (`format.md` §7): no operators, no duplicate keys, no
  * directives; substitutions still unresolved.
  */
private[psi] object Tree {

  sealed trait Value {
    def tags: Set[String]
    def location: PsiLocation
  }

  final case class Scalar(value: String, tags: Set[String], location: SourceLocation) extends Value

  final case class Null(tags: Set[String], location: SourceLocation) extends Value

  final case class Sequence(elements: Vector[Value], tags: Set[String], location: SourceLocation) extends Value

  /** `\${path}` or `\${?path}`, alone and unquoted: a graft. */
  final case class Substitution(path: Id.Branch, optional: Boolean, tags: Set[String], location: SourceLocation)
      extends Value

  /** A scalar with `\${…}` inside it. */
  final case class Concatenation(parts: Vector[Token.Part], tags: Set[String], location: SourceLocation) extends Value

  /** A map, its keys in the order they first appeared (`format.md` §7).
    *
    * @param sites
    *   where it was defined, the latest first: one place, or each block merged into it since it was last replaced
    */
  final case class Map(
      keys: Vector[String],
      entries: immutable.Map[String, Value],
      tags: Set[String],
      sites: ::[SourceLocation])
      extends Value {
    def location: PsiLocation =
      sites.tail match {
        case Nil => sites.head
        case earlier @ ::(_, _) => MergedLocation(sites.head, earlier)
      }

    /** This map with `value` at `key`: in its place when the key is there, last when it is not. */
    def updated(key: String, value: Value): Map =
      copy(keys = if (entries.contains(key)) keys else keys :+ key, entries = entries.updated(key, value))

    /** This map without `key`. */
    def removed(key: String): Map = copy(keys = keys.filter(_ != key), entries = entries - key)
  }

  object Map {
    def empty(tags: Set[String], site: SourceLocation): Map =
      Map(Vector.empty, immutable.Map.empty, tags, ::(site, Nil))
  }
}
