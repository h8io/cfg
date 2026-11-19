package h8io.cfg.impl.psi

import h8io.cfg.{Id, Location}

import scala.annotation.tailrec
import scala.collection.mutable

/** Where a node of a Psi configuration came from.
  *
  * A node written in a source carries a [[SourceLocation]]. A node produced by a substitution carries the reference
  * site as its primary coordinates together with the location of the node it was taken from, so `description` names the
  * line the reader has to go to first and the definition behind it second.
  */
sealed trait PsiLocation extends Location {}

private[psi] object PsiLocation {

  /** The longest description, past which it is cut off with an ellipsis (`format.md` §10). */
  val DescriptionLimit: Int = 10000

  /** The description of `location`, built with a stack of its own rather than by recursion: a chain of substitutions
    * nests a location in another for every link, as deep as the chain is long. It is cut off at [[DescriptionLimit]]
    * characters: locations share the locations they refer to, and a concatenation referring twice to one built the same
    * way, and so on, describes in a length exponential in the chain's.
    */
  def describe(location: Location): String = {
    val out = new java.lang.StringBuilder
    var stack: List[Part] = Right(location) :: Nil
    while (stack.nonEmpty && out.length <= DescriptionLimit) {
      val head = stack.head
      stack = stack.tail
      head match {
        case Left(text) => val _ = out.append(text)
        case Right(next) => stack = parts(next) ::: stack
      }
    }
    if (out.length <= DescriptionLimit) out.toString else out.substring(0, DescriptionLimit) + "…"
  }

  /** `location` written as a case class writes itself, with the same stack and cut off at the same limit as
    * [[describe]]: the `toString` a case class generates would write every location shared as many times as there are
    * paths to it.
    */
  def show(location: PsiLocation): String = {
    val out = new java.lang.StringBuilder
    var stack: List[Either[String, Any]] = Right(location) :: Nil
    while (stack.nonEmpty && out.length <= DescriptionLimit) {
      val head = stack.head
      stack = stack.tail
      head match {
        case Left(text) => val _ = out.append(text)
        case Right(composite @ (_: ReferenceLocation | _: ConcatenationLocation | _: MergedLocation |
            _: DirectiveLocation)) =>
          val product = composite.asInstanceOf[Product]
          stack = Left(product.productPrefix + "(") :: fields(product.productIterator.toList) ::: stack
        case Right(list: List[?]) => stack = Left("List(") :: fields(list) ::: stack
        case Right(other) => val _ = out.append(other.toString)
      }
    }
    if (out.length <= DescriptionLimit) out.toString else out.substring(0, DescriptionLimit) + "…"
  }

  /** Whether `location` equals `that` as the `equals` of a case class tells, with a stack of its own rather than by
    * recursion, and each pair of locations compared once: locations share those they refer to, and two built the same
    * way but apart, as two loads of one source build them, would otherwise be compared once for every path to each
    * pair, in a time exponential in the length of a chain of concatenations.
    */
  def equal(location: PsiLocation, that: Any): Boolean = {
    val compared = mutable.Set.empty[Pair]
    @tailrec def loop(stack: List[(Any, Any)]): Boolean =
      stack match {
        case Nil => true
        case (a, b) :: rest if same(a, b) || !compared.add(new Pair(a, b)) => loop(rest)
        case (a, b) :: rest =>
          (a, b) match {
            case (a: ReferenceLocation, b: ReferenceLocation) =>
              a.hashCode == b.hashCode && a.at == b.at && a.path == b.path && loop((a.origin, b.origin) :: rest)
            case (a: ConcatenationLocation, b: ConcatenationLocation) =>
              a.hashCode == b.hashCode && a.at == b.at && a.parts.size == b.parts.size &&
              loop(a.parts.zip(b.parts) ::: rest)
            case (a: MergedLocation, b: MergedLocation) =>
              a.hashCode == b.hashCode && a.earlier.size == b.earlier.size &&
              loop((a.at, b.at) :: a.earlier.zip(b.earlier) ::: rest)
            case (a: DirectiveLocation, b: DirectiveLocation) =>
              a.hashCode == b.hashCode && a.at == b.at && a.directive == b.directive &&
              loop((a.origin, b.origin) :: rest)
            // a location of one kind and another of another, or anything else
            case _ => !composite(a) && !composite(b) && a == b && loop(rest)
          }
      }
    loop((location, that) :: Nil)
  }

  private def composite(value: Any): Boolean =
    value match {
      case _: ReferenceLocation | _: ConcatenationLocation | _: MergedLocation | _: DirectiveLocation => true
      case _ => false
    }

  private def same(a: Any, b: Any): Boolean = a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]

  /** A pair of values, told apart from another by what they are rather than by what they equal. */
  private final class Pair(val a: Any, val b: Any) {
    // compared with pairs alone, in the set of those compared
    override def equals(that: Any): Boolean = {
      val pair = that.asInstanceOf[Pair]
      same(a, pair.a) && same(b, pair.b)
    }

    override def hashCode: Int = System.identityHashCode(a) * 31 + System.identityHashCode(b)
  }

  /** `values` separated by commas, as a case class separates its fields, and the parenthesis closed. */
  private def fields(values: List[Any]): List[Either[String, Any]] =
    values.zipWithIndex.flatMap { case (value, i) =>
      if (i == 0) Right(value) :: Nil else Left(",") :: Right(value) :: Nil
    } :::
      Left(")") :: Nil

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

  /** Where an error here stands among the errors of a load from `sources`, in order: by source, then by line and
    * column; a source not given, last.
    */
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
  // a location shares those it refers to, which the hash of a case class would compute again for every path to them
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)

  // and would compare them again for every path to them
  override def equals(that: Any): Boolean = PsiLocation.equal(this, that)

  def description: String = PsiLocation.describe(this)

  override def toString: String = PsiLocation.show(this)
}

/** A scalar built at `at` by concatenation, with one reference per `\${…}` in the order written. */
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends PsiLocation {
  // a location shares those it refers to, which the hash of a case class would compute again for every path to them
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)

  // and would compare them again for every path to them
  override def equals(that: Any): Boolean = PsiLocation.equal(this, that)

  def description: String = PsiLocation.describe(this)

  override def toString: String = PsiLocation.show(this)
}

/** A map defined in more places than one, merged (`format.md` §7, §10): `at` is where it was defined last, `earlier`
  * the places before, the latest first.
  */
final case class MergedLocation(at: PsiLocation, earlier: ::[PsiLocation]) extends PsiLocation {
  // a location shares those it refers to, which the hash of a case class would compute again for every path to them
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)

  // and would compare them again for every path to them
  override def equals(that: Any): Boolean = PsiLocation.equal(this, that)

  def description: String = PsiLocation.describe(this)

  override def toString: String = PsiLocation.show(this)
}

/** The root of a load given no source at all, an empty map that was written nowhere. */
case object NoSourceLocation extends PsiLocation {
  def description: String = "no source"
}

/** A node a directive built from a `Node` of its own and placed at `at`, the call (`format.md` §6, §10).
  *
  * @param directive
  *   the directive's name, rendered after `#` as a key is, in backticks when it is not bare
  * @param origin
  *   the location the node had
  */
final case class DirectiveLocation(at: SourceLocation, directive: String, origin: Location) extends PsiLocation {
  // a location shares those it refers to, which the hash of a case class would compute again for every path to them
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)

  // and would compare them again for every path to them
  override def equals(that: Any): Boolean = PsiLocation.equal(this, that)

  def description: String = PsiLocation.describe(this)

  override def toString: String = PsiLocation.show(this)
}

/** A value `#prop` read from the system property `name` (`format.md` §6, §10). */
final case class SystemPropertyLocation(name: String) extends PsiLocation {
  def description: String = "system property " + name
}

/** A value `#env` read from the environment variable `name` (`format.md` §6, §10). */
final case class EnvironmentLocation(name: String) extends PsiLocation {
  def description: String = "environment variable " + name
}
