package h8io.cfg.impl.psi

import h8io.cfg.{Id, Location}

/** Where a node of a Psi configuration came from.
  *
  * A node written in a source carries a [[SourceLocation]]. A node produced by a substitution carries the reference
  * site as its primary coordinates together with the location of the node it was taken from, so `description` names the
  * line the reader has to go to first and the definition behind it second.
  */
sealed trait PsiLocation extends Location {

  /** Where an error at this location stands among the errors of a load from `sources`, in order: by source, then by
    * line and column; a location in no source given, last.
    */
  private[psi] def order(sources: Seq[String]): (Int, Int, Int)
}

/** A position in a source: the source as it was named, a 1-based line and a 1-based column counted in code points.
  *
  * @param source
  *   the URL passed to the loader, or an include argument as written in the directive
  */
final case class SourceLocation(source: String, line: Int, column: Int) extends PsiLocation {
  def description: String = source + ':' + line.toString + ':' + column.toString

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = {
    val index = sources.indexOf(source)
    (if (index < 0) sources.size else index, line, column)
  }
}

/** A node grafted at `at` by `\${path}`.
  *
  * @param origin
  *   the location of the node the value was taken from; a chain of substitutions nests further references here
  */
final case class ReferenceLocation(at: SourceLocation, path: String, origin: Location) extends PsiLocation {
  def description: String = at.description + " (" + reference + ')'

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)

  /** The reference without its own site, as a part of a concatenation renders it. */
  private[psi] def reference: String = "${" + path + "} from " + origin.description
}

/** A scalar built at `at` by concatenation, with one reference per `\${…}` in the order written. */
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends PsiLocation {
  def description: String = parts.iterator.map(_.reference).mkString(at.description + " (", ", ", ")")

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)
}

/** A map defined in more places than one, merged (`format.md` §7, §10): `at` is where it was defined last, `earlier`
  * the places before, the latest first.
  */
final case class MergedLocation(at: PsiLocation, earlier: ::[PsiLocation]) extends PsiLocation {
  def description: String = earlier.iterator.map(_.description).mkString(at.description + " (also ", ", ", ")")

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)
}

/** The root of a load given no source at all, an empty map that was written nowhere. */
case object NoSourceLocation extends PsiLocation {
  def description: String = "no source"

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = (sources.size, 0, 0)
}

/** A node a directive built from a `Node` of its own and placed at `at`, the call (`format.md` §6, §10).
  *
  * @param directive
  *   the directive's name, rendered after `#` as a key is, in backticks when it is not bare
  * @param origin
  *   the location the node had
  */
final case class DirectiveLocation(at: SourceLocation, directive: String, origin: Location) extends PsiLocation {
  def description: String =
    at.description + " (#" + Id.Key(directive, Id.Root).path + " from " + origin.description + ')'

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)
}

/** A value overridden at launch by the system property `name`. */
final case class SystemPropertyLocation(name: String) extends PsiLocation {
  def description: String = "system property " + name

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = (sources.size, 0, 0)
}
