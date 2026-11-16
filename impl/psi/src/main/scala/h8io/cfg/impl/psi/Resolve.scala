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
  *
  * What resolution adds to the tree is limited too (§11): at most `nodeLimit` nodes copied by grafts, and
  * `characterLimit` characters in the scalars concatenations build, each counted before it is built. Past either, the
  * load fails: the first to go past is reported, and nothing is built after it.
  */
private[psi] final class Resolve(root: Tree.Map, limit: Int, nodeLimit: Int, characterLimit: Int) {
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

  /** The nodes grafts have copied, and the characters of the scalars concatenations have built. */
  private var nodes = 0L
  private var characters = 0L

  /** Whether either has gone past its limit, after which nothing more is built. */
  private var exhausted = false

  /** Merges what injections bring into the maps they stand in, as it would have been merged had it been written there.
    */
  private val merging = new Merge

  def tree: Result[Tree.Map] = {
    // every path goes through the root: an injection there would refer to the tree it is building (§9)
    errors ++= root.steps.collect { case Tree.Step.Injection(_, _, at) => InjectionAtRoot(at) }
    val resolved = if (root.steps.isEmpty) map(Id.Root, root).toOption else None
    Result(resolved.getOrElse(root), errors.toList ++ merging.reported)
  }

  /** The value at `id` resolved: `Right(None)` when it is an optional substitution with nothing to graft. */
  private def resolve(id: Id, value: Tree.Value): Resolution =
    value match {
      case _: Tree.Scalar | _: Tree.Null => Right(Some(value))
      case map: Tree.Map if map.steps.nonEmpty => step(completed(id, map).flatMap(this.map(id, _)).map(Some(_)))
      case map: Tree.Map => step(this.map(id, map).map(Some(_)))
      case sequence: Tree.Sequence => step(this.sequence(id, sequence).map(Some(_)))
      case injection: Tree.Injection =>
        once(id, Some(new Following(injection.location, injection.path)))(injected(id, injection))
      case Tree.Substitution(path, optional, tags, at) =>
        once(id, Some(new Following(at, path)))(
          reference(path, optional, at).flatMap(_.fold[Resolution](Right(None))(grafted(id, _, at, path, tags))))
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
    val elements = sequence.elements.zipWithIndex.map { case (value, i) => value -> resolve(Id.Index(i, id), value) }
    failure(elements.map(_._2)).toLeft(sequence.copy(elements = elements.flatMap {
      // an injection puts the elements of a sequence it found
      case (_: Tree.Injection, Right(Some(found: Tree.Sequence))) => found.elements
      case (_, Right(Some(value))) => Vector(value)
      case _ => Vector.empty
    }))
  }

  /** `map`, which has steps, completed once: each injection resolved and merged in turn with what is written after it,
    * as if what it found were written there (§9). Its children are resolved apart, after it, so that one may refer to
    * another; only what an injection refers to through the map itself leads back to it.
    *
    * An injection that fails fails the map. The injections after it are resolved all the same, so that each reports its
    * own errors (§11), but nothing more is merged into the map: what the failed one would have brought is unknown, and
    * a conflict with what follows it would be no root cause.
    */
  private def completed(id: Id, map: Tree.Map): Either[Failure, Tree.Map] =
    once(id, map.steps.collectFirst { case Tree.Step.Injection(path, _, at) => new Following(at, path) }) {
      val failures = mutable.ListBuffer.empty[Either[Failure, Unit]]
      val done = map.steps.foldLeft(map.copy(steps = Vector.empty)) { (current, step) =>
        step match {
          case Tree.Step.Injection(path, optional, at) =>
            // after a failure, into nothing, for its own errors alone
            val into = if (failures.isEmpty) current else Tree.Map.empty(Set.empty, at)
            fields(id, into, path, optional, at) match {
              case Right(merged) if failures.isEmpty => merged
              case Right(_) => current
              case Left(failure) =>
                failures += Left(failure)
                current
            }
          case _ if failures.nonEmpty => current
          case Tree.Step.Statements(statements) => merging.block(current, statements)
          case Tree.Step.Graft(content) => over(current, content)
        }
      }
      failure(failures.toList).toLeft(Some(done))
      // a completion always gives a map
    }.map(_.get.asInstanceOf[Tree.Map])

  /** The map `current` with the fields of the map at `path` merged into it; unchanged when an optional injection finds
    * nothing.
    */
  private def fields(
      id: Id,
      current: Tree.Map,
      path: Id.Branch,
      optional: Boolean,
      at: SourceLocation): Either[Failure, Tree.Map] =
    reference(path, optional, at).flatMap(_.fold[Either[Failure, Tree.Map]](Right(current)) {
      case found: Tree.Map =>
        // its fields stand where the map's do
        if (segments(id).size - 1 + height(found) > limit) failed(DepthExceeded(at, limit, fatal = false))
        else
          grow(size(found) - 1, 0, ResolutionTooLarge(at, Message.path(path), nodeLimit))
            // a map grafted is a map
            .map(_ => over(current, graft(found, at, path, Set.empty).asInstanceOf[Tree.Map]))
      case other => failed(InjectionNotMap(at, Message.path(path), other.location))
    })

  /** `value`, found by `\${path}` at `at`, grafted at `id` with the tags written before it added to its own. */
  private def grafted(id: Id, value: Tree.Value, at: SourceLocation, path: Id.Branch, tags: Set[String]): Resolution =
    // the levels around the substitution, and those its value opens
    if (segments(id).size - 1 + height(value) > limit) failed(DepthExceeded(at, limit, fatal = false))
    else
      grow(size(value), 0, ResolutionTooLarge(at, Message.path(path), nodeLimit))
        .map(_ => Some(graft(value, at, path, tags)))

  /** What an injection among elements finds: a sequence, whose elements are to stand in its place, or anything else, to
    * stand there as one element; none when an optional one finds nothing.
    */
  private def injected(id: Id, injection: Tree.Injection): Resolution = {
    val Tree.Injection(path, optional, tags, at) = injection
    reference(path, optional, at).flatMap(_.fold[Resolution](Right(None)) {
      // the tags have no node to go to
      case found: Tree.Sequence if tags.nonEmpty => failed(TaggedInjection(at, Message.path(path), found.location))
      case found: Tree.Sequence =>
        // its elements stand where the injection does
        if (segments(id).size - 2 + height(found) > limit) failed(DepthExceeded(at, limit, fatal = false))
        else
          grow(size(found) - 1, 0, ResolutionTooLarge(at, Message.path(path), nodeLimit))
            .map(_ => Some(graft(found, at, path, Set.empty)))
      case found => grafted(id, found, at, path, tags)
    })
  }

  /** `current` with the fields of `content`, resolved, merged into it as `:` merges them (§7): maps merge, anything
    * else replaces, and a map over a substitution is an error, the substitution maybe no map.
    */
  private def over(current: Tree.Map, content: Tree.Map): Tree.Map =
    content.keys.foldLeft(current) { (map, key) =>
      val value = content.entries(key)
      map.entries.get(key).fold(map.updated(key, value)) { earlier =>
        combined(earlier, value).fold(map.failedAt(key))(map.updated(key, _))
      }
    }

  private def combined(earlier: Tree.Value, value: Tree.Value): Option[Tree.Value] =
    (earlier, value) match {
      case (map: Tree.Map, content: Tree.Map) if map.steps.isEmpty =>
        Some(over(
          map.copy(tags = map.tags ++ content.tags, location = Merge.again(content.location, map.location)), content))
      // a map with injections of its own takes this one after them
      case (map: Tree.Map, content: Tree.Map) =>
        Some(map.copy(
          tags = map.tags ++ content.tags,
          location = Merge.again(content.location, map.location),
          steps = map.steps :+ Tree.Step.Graft(content)))
      case (substitution: Tree.Substitution, content: Tree.Map) =>
        errors += MergeWithSubstitution(content.location, substitution.location)
        None
      case _ => Some(value)
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
          // what an injection brings in among fields is known only once it is resolved
          case map: Tree.Map if map.steps.nonEmpty => completed(id, map).map(Some(_))
          case _ => Right(Some(value))
        }
        here.flatMap(_.fold[Either[Failure, Option[(Id, Tree.Value)]]](Right(None))(child(id, _, next))) match {
          case Right(Some((childId, child))) => find(childId, child, rest)
          case Right(None) => Right(None)
          case Left(failure) => Left(failure)
        }
    }

  /** The child of `container` at `id` that `segment` names, with its position in the tree. An injection before it in a
    * sequence is resolved, to know how many elements it puts, and one that puts several gives its own.
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
              case injection: Tree.Injection =>
                resolve(Id.Index(i, id), injection) match {
                  case Right(None) => element(i + 1, left)
                  case Right(Some(found: Tree.Sequence)) =>
                    if (left < found.elements.size) Right(Some(Id.Index(i, id) -> found.elements(left)))
                    else element(i + 1, left - found.elements.size)
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
      case part: Token.Reference =>
        val (path, optional, at) = (part.path, part.optional, part.location)
        reference(path, optional, at).flatMap {
          case Some(Tree.Scalar(text, _, origin)) =>
            Right(text -> Some(ReferenceLocation(at, Message.path(path), origin)))
          case Some(other) => failed(NonScalarInConcatenation(at, Message.path(path), other.location))
          case None => Right("" -> None)
        }
    }
    failure(parts).toLeft(parts.collect { case Right(part) => part }).flatMap { parts =>
      val (texts, references) = parts.unzip
      grow(0, texts.foldLeft(0L)(_ + _.length), ConcatenationTooLong(concatenation.location, characterLimit)).map { _ =>
        val location = references.flatten.toList match {
          case Nil => concatenation.location
          case first :: rest => ConcatenationLocation(concatenation.location, ::(first, rest))
        }
        Tree.Scalar(texts.mkString, concatenation.tags, location)
      }
    }
  }

  /** Counts `nodes` and `characters` more added to the tree, before they are built; past either limit the load fails,
    * with `error` the first time and silently after, nothing more being built.
    */
  private def grow(nodes: Long, characters: Long, error: => PsiError): Either[Failure, Unit] =
    if (exhausted) Left(Failed)
    else if (this.nodes + nodes > nodeLimit || this.characters + characters > characterLimit) {
      exhausted = true
      failed(error)
    } else {
      this.nodes += nodes
      this.characters += characters
      Right(())
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

  def apply(
      root: Tree.Map,
      limit: Int = PsiLoader.DefaultDepthLimit,
      nodeLimit: Int = PsiLoader.DefaultNodeLimit,
      characterLimit: Int = PsiLoader.DefaultCharacterLimit): Result[Tree.Map] =
    new Resolve(root, limit, nodeLimit, characterLimit).tree

  /** How many nodes `value` holds, itself among them. */
  private def size(value: Tree.Value): Long =
    value match {
      case map: Tree.Map => map.entries.valuesIterator.foldLeft(1L)(_ + size(_))
      case sequence: Tree.Sequence => sequence.elements.foldLeft(1L)(_ + size(_))
      case _ => 1L
    }

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
        case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Marked | _: Tree.Injection =>
          throw new IllegalStateException("An unresolved substitution at " + value.location.description + " grafted")
      }
    }
    moved(value, path, tags)
  }
}
