package h8io.cfg.impl.psi

import h8io.cfg.Id

import scala.annotation.tailrec
import scala.collection.mutable

/** Resolves the substitutions of a merged tree (`format.md` §9), its values already overridden at launch, giving a tree
  * with none left.
  *
  * A path is looked up in the tree alone. A substitution on the way is resolved first, to look into what it grafts; the
  * node found is resolved whole before it is grafted. Each substitution of the tree is resolved once, by its position,
  * and the position is marked while it is being resolved: reaching it again is a cycle, reported by the reference that
  * reached it.
  *
  * Errors are root causes only (§11): a node that depends on a failed one fails silently, and so does every container
  * holding it — the result is then discarded anyway.
  *
  * Resolution goes at most `limit` steps deep, each a level descended or a substitution followed, and a value is
  * grafted only where it fits under `limit` levels (§11). A substitution already resolved counts as deep as resolving
  * it went, so that what goes past the limit does not depend on the order the tree is resolved in.
  */
private[psi] final class Resolve(root: Tree.Map, limit: Int) {
  import Resolve.*

  private val errors = mutable.ListBuffer.empty[PsiError]

  /** How many steps deep resolution is: levels descended below the root and substitutions followed. */
  private var steps = 0

  /** The substitutions being followed, innermost first. */
  private var following: List[Following] = Nil

  /** The deepest `steps` has gone since the resolution being measured began, a memoized one counted as deep as
    * resolving it went.
    */
  private var deepest = 0

  /** By position, a substitution resolved, with how many steps deeper than where it began resolving it went; `None`
    * while it is being resolved.
    */
  private val resolved = mutable.Map.empty[Id, Option[(Resolution, Int)]]

  def tree: Result[Tree.Map] = Result(map(Id.Root, root).getOrElse(root), errors.toList)

  /** The value at `id` resolved: `Right(None)` when it is an optional substitution with nothing to graft. */
  private def resolve(id: Id, value: Tree.Value): Resolution =
    value match {
      case _: Tree.Scalar | _: Tree.Null => Right(Some(value))
      case map: Tree.Map => step(this.map(id, map).map(Some(_)))
      case sequence: Tree.Sequence => step(this.sequence(id, sequence).map(Some(_)))
      case Tree.Substitution(path, optional, tags, at) =>
        once(id, Some(new Following(at, path)))(reference(path, optional, at).flatMap(_.fold[Resolution](Right(None)) {
          value =>
            // the levels around the substitution, and those its value opens
            if (segments(id).size - 1 + height(value) <= limit) Right(Some(graft(value, at, path, tags)))
            else failed(DepthExceeded(at, limit, fatal = false))
        }))
      case concatenation: Tree.Concatenation =>
        once(id, first(concatenation))(this.concatenation(concatenation).map(Some(_)))
      case Tree.Marked(value, markers) =>
        // resolved inside `once`, so that a cycle through the block is caught and the block resolved only once
        once(id, Some(value).collect { case concatenation: Tree.Concatenation => concatenation }.flatMap(first)) {
          val text = value match {
            case concatenation: Tree.Concatenation => this.concatenation(concatenation)
            case scalar: Tree.Scalar => Right(scalar)
            // the parser marks quoted blocks only, which are scalars or concatenations
            case other => throw new IllegalStateException("Markers on a non-scalar at " + other.location.description)
          }
          text.map(scalar => Some(marked(scalar, markers)))
        }
    }

  /** `scalar` with the loader's markers run over its value in turn (§5); with none of them when one fails. */
  private def marked(scalar: Tree.Scalar, markers: Vector[MarkerParser.MarkerCall]): Tree.Scalar =
    markers.foldLeft[Either[PsiError, String]](Right(scalar.value)) { (text, call) =>
      text.flatMap(text => call.marker(call.arguments, text).left.map(MarkerFailed(call.location, call.marker.name, _)))
    } match {
      case Right(text) => scalar.copy(value = text)
      case Left(error) =>
        errors += error
        scalar
    }

  private def map(id: Id, map: Tree.Map): Either[Failure, Tree.Map] = {
    val entries = map.keys.map(key => key -> resolve(Id.Key(key, id), map.entries(key)))
    // a key whose merge failed fails the map, after its other children have reported their own errors
    failure(entries.map(_._2) ++ map.failed.headOption.map(_ => Left(Failed))).toLeft {
      val present = entries.collect { case (key, Right(Some(value))) => key -> value }
      map.copy(keys = present.map(_._1), entries = present.toMap)
    }
  }

  private def sequence(id: Id, sequence: Tree.Sequence): Either[Failure, Tree.Sequence] = {
    val elements = sequence.elements.zipWithIndex.map { case (value, i) => resolve(Id.Index(i, id), value) }
    failure(elements).toLeft(sequence.copy(elements = elements.collect { case Right(Some(value)) => value }))
  }

  /** The node at `id`, resolved once: a later use takes what the first gave, as many steps deep as it went, and fails
    * where resolving it again would go past the limit, as `step` does; with no substitution being followed, at `site`,
    * the node's own first reference.
    */
  private def once(id: Id, site: Option[Following])(resolution: => Resolution): Resolution =
    resolved.get(id) match {
      case Some(Some((done, height))) if done.isLeft || steps + height <= limit =>
        deepest = math.max(deepest, steps + height)
        done
      case Some(Some(_)) => tooDeep(following.headOption.orElse(site))
      case Some(None) => Left(Cycle)
      case None =>
        resolved(id) = None
        val outer = deepest
        deepest = steps
        val done = resolution
        resolved(id) = Some((done, deepest - steps))
        deepest = math.max(outer, deepest)
        done
    }

  /** `resolution`, a step deeper; past the limit the innermost substitution being followed fails. The tree's own levels
    * being within the limit, there is one whenever it is gone past.
    */
  private def step(resolution: => Resolution): Resolution =
    if (steps >= limit) tooDeep(following.headOption)
    else {
      steps += 1
      deepest = math.max(deepest, steps)
      try resolution
      finally steps -= 1
    }

  /** A failure past the limit, reported at `site` once however many of its branches go past. */
  private def tooDeep(site: Option[Following]): Resolution = {
    site.filterNot(_.reported).foreach { site =>
      site.reported = true
      errors += ResolutionTooDeep(site.at, Message.path(site.path), limit)
    }
    Left(Failed)
  }

  /** `\${path}` or `\${?path}` at `at`: the node it refers to, resolved, or `None` for an optional one with none. */
  private def reference(path: Id.Branch, optional: Boolean, at: SourceLocation): Resolution = {
    following = new Following(at, path) :: following
    try
      step(lookup(path)) match {
        case Left(Cycle) => failed(SubstitutionCycle(at, Message.path(path)))
        case Right(None) if !optional => failed(UnresolvedSubstitution(at, Message.path(path)))
        case other => other
      }
    finally following = following.tail
  }

  private def failed[A](error: PsiError): Either[Failure, A] = {
    errors += error
    Left(Failed)
  }

  private def lookup(path: Id.Branch): Resolution = find(Id.Root, root, Resolve.segments(path))

  /** The node at `segments` below `value` at `id`, resolved; looked up a segment at a time, not by recursion. */
  @tailrec private def find(id: Id, value: Tree.Value, segments: List[Id.Branch]): Resolution =
    segments match {
      case Nil => resolve(id, value)
      case next :: rest =>
        val here = value match {
          case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Marked => resolve(id, value)
          case _ => Right(Some(value))
        }
        here.flatMap(_.fold[Either[Failure, Option[(Id, Tree.Value)]]](Right(None))(child(id, _, next))) match {
          case Right(Some((childId, child))) => find(childId, child, rest)
          case Right(None) => Right(None)
          case Left(failure) => Left(failure)
        }
    }

  /** The child of `container` at `id` that `segment` names, with its position in the tree. An optional substitution
    * before it in a sequence is resolved, to know whether it drops out and moves the rest up.
    */
  private def child(id: Id, container: Tree.Value, segment: Id.Branch): Either[Failure, Option[(Id, Tree.Value)]] =
    (container, segment) match {
      case (map: Tree.Map, Id.Key(key, _)) =>
        map.entries.get(key) match {
          case Some(value) => Right(Some(Id.Key(key, id) -> value))
          case None => if (map.failed(key)) Left(Failed) else Right(None)
        }
      case (sequence: Tree.Sequence, Id.Index(index, _)) =>
        @tailrec def element(i: Int, left: Int): Either[Failure, Option[(Id, Tree.Value)]] =
          if (i == sequence.elements.size) Right(None)
          else
            sequence.elements(i) match {
              case substitution: Tree.Substitution if substitution.optional =>
                resolve(Id.Index(i, id), substitution) match {
                  case Right(None) => element(i + 1, left)
                  case Right(Some(_)) if left > 0 => element(i + 1, left - 1)
                  case Right(Some(value)) => Right(Some(Id.Index(i, id) -> value))
                  case Left(failure) => Left(failure)
                }
              case value if left == 0 => Right(Some(Id.Index(i, id) -> value))
              case _ => element(i + 1, left - 1)
            }
        element(0, index)
      case _ => Right(None)
    }

  /** A scalar of text and `\${…}`, each referring to a scalar; an optional one with nothing to refer to is empty. */
  private def concatenation(concatenation: Tree.Concatenation): Either[Failure, Tree.Scalar] = {
    val parts = concatenation.parts.map {
      case Token.Text(text) => Right(text -> None)
      case Token.Reference(path, optional, at) =>
        reference(path, optional, at).flatMap {
          case Some(Tree.Scalar(text, _, origin)) =>
            Right(text -> Some(ReferenceLocation(at, Message.path(path), origin)))
          case Some(other) => failed(NonScalarInConcatenation(at, Message.path(path), other.location))
          case None => Right("" -> None)
        }
    }
    failure(parts).toLeft {
      val (texts, references) = parts.collect { case Right(part) => part }.unzip
      val location = references.flatten.toList match {
        case Nil => concatenation.location
        case first :: rest => ConcatenationLocation(concatenation.location, ::(first, rest))
      }
      Tree.Scalar(texts.mkString, concatenation.tags, location)
    }
  }
}

private[psi] object Resolve {

  /** Why a node did not resolve: it failed, its error already reported, or it is part of a cycle not reported yet. */
  sealed trait Failure

  case object Failed extends Failure

  case object Cycle extends Failure

  type Resolution = Either[Failure, Option[Tree.Value]]

  /** A substitution being followed, written at `at`, and whether it has been reported for going too deep. */
  private final class Following(val at: SourceLocation, val path: Id.Branch) {
    var reported = false
  }

  /** The first reference of `concatenation`, where it is reported going past the limit with no substitution followed.
    */
  private def first(concatenation: Tree.Concatenation): Option[Following] =
    concatenation.parts.collectFirst { case Token.Reference(path, _, at) => new Following(at, path) }

  def apply(root: Tree.Map, limit: Int = PsiLoader.DefaultDepthLimit): Result[Tree.Map] = new Resolve(root, limit).tree

  /** How many levels `value` opens. */
  private def height(value: Tree.Value): Int =
    value match {
      case map: Tree.Map => 1 + map.entries.valuesIterator.map(height).foldLeft(0)(math.max)
      case sequence: Tree.Sequence => 1 + sequence.elements.iterator.map(height).foldLeft(0)(math.max)
      case _ => 0
    }

  /** The failure of a container: a cycle if any of its children is in one, to be reported by the reference closing it.
    */
  private def failure(children: Seq[Either[Failure, Any]]): Option[Failure] = {
    val failures = children.collect { case Left(failure) => failure }
    if (failures.contains(Cycle)) Some(Cycle) else failures.headOption
  }

  private def segments(id: Id): List[Id.Branch] = {
    @tailrec def loop(id: Id, segments: List[Id.Branch]): List[Id.Branch] =
      id match {
        case branch: Id.Branch => loop(branch.parent, branch :: segments)
        case _: Id.Root => segments
      }
    loop(id, Nil)
  }

  /** `value` grafted at `at` by `\${path}`, with the tags written before it added to its own (§9, §10): every node of
    * it keeps its origin under a reference to the path it was taken by.
    */
  private[psi] def graft(value: Tree.Value, at: SourceLocation, path: Id.Branch, tags: Set[String]): Tree.Value = {
    def moved(value: Tree.Value, id: Id.Branch, tags: Set[String]): Tree.Value = {
      val location = ReferenceLocation(at, id.path, value.location)
      value match {
        case scalar: Tree.Scalar => scalar.copy(tags = scalar.tags ++ tags, location = location)
        case node: Tree.Null => node.copy(tags = node.tags ++ tags, location = location)
        case sequence: Tree.Sequence =>
          val elements = sequence.elements.zipWithIndex.map { case (e, i) => moved(e, Id.Index(i, id), Set.empty) }
          sequence.copy(elements = elements, tags = sequence.tags ++ tags, location = location)
        case map: Tree.Map =>
          val entries = map.entries.map { case (key, child) => key -> moved(child, Id.Key(key, id), Set.empty) }
          map.copy(entries = entries, tags = map.tags ++ tags, location = location)
        // a node is resolved whole before it is grafted
        case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Marked =>
          throw new IllegalStateException("An unresolved substitution at " + value.location.description + " grafted")
      }
    }
    moved(value, path, tags)
  }
}
