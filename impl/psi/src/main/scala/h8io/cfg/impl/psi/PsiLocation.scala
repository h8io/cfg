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

  /** The position in a source this location starts with, which an error report groups it under; none for a location in
    * no source.
    */
  private[psi] def anchor: Option[SourceLocation]
}

private[psi] object PsiLocation {

  /** The description of `location`, built with a stack of its own rather than by recursion: a chain of substitutions
    * nests a location in another for every link, as deep as the chain is long, and no depth limit bounds a chain
    * written in order.
    */
  def describe(location: Location): String = {
    val out = new java.lang.StringBuilder
    var stack: List[Part] = Right(location) :: Nil
    while (stack.nonEmpty) {
      val head = stack.head
      stack = stack.tail
      head match {
        case Left(text) => val _ = out.append(text)
        case Right(next) => stack = parts(next) ::: stack
      }
    }
    out.toString
  }

  /** A part of a description: text as it is, or a location to describe in turn. */
  private type Part = Either[String, Location]

  /** What the description of `location` is made of, in order. */
  private def parts(location: Location): List[Part] =
    location match {
      case ReferenceLocation(at, path, origin) => Left(at.description + " (") :: reference(path, origin) :::
          Left(")") :: Nil
      case ConcatenationLocation(at, parts) =>
        Left(at.description + " (") :: joined(parts.map(part => reference(part.path, part.origin)))
      case MergedLocation(at, earlier) => Right(at) :: Left(" (also ") :: joined(earlier.map(Right(_) :: Nil))
      case DirectiveLocation(at, directive, origin) =>
        Left(at.description + " (#" + Id.Key(directive, Id.Root).path + " from ") :: Right(origin) :: Left(")") :: Nil
      case _ => Left(location.description) :: Nil
    }

  private def reference(path: String, origin: Location): List[Part] =
    Left("${" + path + "} from ") :: Right(origin) :: Nil

  /** `items` separated by commas, and the parenthesis closed. */
  private def joined(items: List[List[Part]]): List[Part] =
    items.head ::: items.tail.flatMap(Left(", ") :: _) ::: Left(")") :: Nil
}

/** A position in a source: the source's name, a 1-based line and a 1-based column counted in code points.
  *
  * @param source
  *   the source's full URL — as passed to the loader, or as a directive's target resolves — or, for a source given as
  *   text, the name it was given
  */
final case class SourceLocation(source: String, line: Int, column: Int) extends PsiLocation {
  def description: String = source + ':' + line.toString + ':' + column.toString

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = {
    val index = sources.indexOf(source)
    (if (index < 0) sources.size else index, line, column)
  }

  private[psi] def anchor: Option[SourceLocation] = Some(this)
}

/** A node grafted at `at` by `\${path}`.
  *
  * @param origin
  *   the location of the node the value was taken from; a chain of substitutions nests further references here
  */
final case class ReferenceLocation(at: SourceLocation, path: String, origin: Location) extends PsiLocation {
  def description: String = PsiLocation.describe(this)

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)

  private[psi] def anchor: Option[SourceLocation] = Some(at)
}

/** A scalar built at `at` by concatenation, with one reference per `\${…}` in the order written. */
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends PsiLocation {
  def description: String = PsiLocation.describe(this)

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)

  private[psi] def anchor: Option[SourceLocation] = Some(at)
}

/** A map defined in more places than one, merged (`format.md` §7, §10): `at` is where it was defined last, `earlier`
  * the places before, the latest first.
  */
final case class MergedLocation(at: PsiLocation, earlier: ::[PsiLocation]) extends PsiLocation {
  def description: String = PsiLocation.describe(this)

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)

  private[psi] def anchor: Option[SourceLocation] = at.anchor
}

/** The root of a load given no source at all, an empty map that was written nowhere. */
case object NoSourceLocation extends PsiLocation {
  def description: String = "no source"

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = (sources.size, 0, 0)

  private[psi] def anchor: Option[SourceLocation] = None
}

/** A node a directive built from a `Node` of its own and placed at `at`, the call (`format.md` §6, §10).
  *
  * @param directive
  *   the directive's name, rendered after `#` as a key is, in backticks when it is not bare
  * @param origin
  *   the location the node had
  */
final case class DirectiveLocation(at: SourceLocation, directive: String, origin: Location) extends PsiLocation {
  def description: String = PsiLocation.describe(this)

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = at.order(sources)

  private[psi] def anchor: Option[SourceLocation] = Some(at)
}

/** A value `#prop` read from the system property `name` (`format.md` §6, §10). */
final case class SystemPropertyLocation(name: String) extends PsiLocation {
  def description: String = "system property " + name

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = (sources.size, 0, 0)

  private[psi] def anchor: Option[SourceLocation] = None
}

/** A value `#env` read from the environment variable `name` (`format.md` §6, §10). */
final case class EnvironmentLocation(name: String) extends PsiLocation {
  def description: String = "environment variable " + name

  private[psi] def order(sources: Seq[String]): (Int, Int, Int) = (sources.size, 0, 0)

  private[psi] def anchor: Option[SourceLocation] = None
}
