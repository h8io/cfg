package h8io.cfg.impl.defg

import h8io.cfg.Location

/** Where a node of a DEF configuration came from.
  *
  * A node written in a source carries a [[SourceLocation]]. A node produced by a substitution carries the reference
  * site as its primary coordinates together with the location of the node it was taken from, so `description` names the
  * line the reader has to go to first and the definition behind it second.
  */
sealed trait DefLocation extends Location

/** A position in a source: the source as it was named, a 1-based line and a 1-based column counted in code points.
  *
  * @param source
  *   the URL passed to the loader, or an include argument as written in the directive
  */
final case class SourceLocation(source: String, line: Int, column: Int) extends DefLocation {
  def description: String = source + ':' + line.toString + ':' + column.toString
}

/** A node grafted at `at` by `\${path}`.
  *
  * @param origin
  *   the location of the node the value was taken from; a chain of substitutions nests further references here
  */
final case class ReferenceLocation(at: SourceLocation, path: String, origin: Location) extends DefLocation {
  def description: String = at.description + " (" + reference + ')'

  /** The reference without its own site, as a part of a concatenation renders it. */
  private[defg] def reference: String = "${" + path + "} from " + origin.description
}

/** A scalar built at `at` by concatenation, with one reference per `\${…}` in the order written. */
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends DefLocation {
  def description: String = parts.iterator.map(_.reference).mkString(at.description + " (", ", ", ")")
}

/** A value read from the system property `name`. */
final case class SystemPropertyLocation(name: String) extends DefLocation {
  def description: String = "system property " + name
}

/** A value read from the environment variable `name`. */
final case class EnvLocation(name: String) extends DefLocation {
  def description: String = "env " + name
}
