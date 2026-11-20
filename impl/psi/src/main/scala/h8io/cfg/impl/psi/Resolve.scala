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

  /** By position, the fields written after an injection that failed, merged into nothing, those between two injections
    * apart: resolved once the map has failed, for their own errors alone.
    */
  private val abandoned = mutable.Map.empty[Id, List[Tree.Map]]

  def tree: Result[Tree.Map] = {
    // every path goes through the root: an injection there would refer to the tree it is building (§9)
    errors ++= root.steps.collect { case Tree.Step.Injection(_, _, at) => InjectionAtRoot(at) }
    val resolved = if (root.steps.isEmpty) map(Id.Root, root).toOption else None
    Result(resolved.getOrElse(root), errors.toList)
  }

  /** The value at `id` resolved: `Right(None)` when it is an optional substitution with nothing to graft. */
  private def resolve(id: Id, value: Tree.Value): Resolution =
    value match {
      case _: Tree.Scalar | _: Tree.Null => Right(Some(value))
      case map: Tree.Map if map.steps.nonEmpty => step(finished(id, completed(id, map)))
      case map: Tree.Map => step(this.map(id, map).map(Some(_)))
      case sequence: Tree.Sequence => step(this.sequence(id, sequence).map(Some(_)))
      case injection: Tree.Injection =>
        once(id, Some(new Following(injection.location, injection.path)))(injected(id, injection))
      case injections: Tree.Injections =>
        once(id, injections.items.collectFirst { case Right(r) => new Following(r.location, r.path) })(
          gathered(id, injections))
      case layers: Tree.Layers =>
        once(id, layers.layers.collectFirst { case Tree.Substitution(path, _, _, at) => new Following(at, path) })(
          layered(id, layers))
      case Tree.Substitution(path, optional, tag, at) =>
        once(id, Some(new Following(at, path)))(
          reference(path, optional, at).flatMap(_.fold[Resolution](Right(None))(grafted(id, _, at, path, tag))))
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
    failure(entries.map(_._2)).toLeft {
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
    * An injection that fails fails the map. What follows it is resolved all the same, so that it reports its own errors
    * (§11), but nothing more is merged into the map: what the failed one would have brought is unknown, and a conflict
    * with what follows it would be no root cause. The injections after it are resolved into nothing, and the fields
    * between two of them are merged into nothing and kept in `abandoned`, to be resolved once the map has failed, as
    * its children would be: what an injection between them would have brought is no more known.
    */
  private def completed(id: Id, map: Tree.Map): Either[Failure, Tree.Map] =
    once(id, map.steps.collectFirst { case Tree.Step.Injection(path, _, at) => new Following(at, path) })(
      complete(id, map).map(Some(_))
      // a completion always gives a map
    ).map(_.get.asInstanceOf[Tree.Map])

  /** `map` at `id` completed, as [[completed]] does, not marked as being resolved: its caller marks it. */
  private def complete(id: Id, map: Tree.Map): Either[Failure, Tree.Map] = {
    val failures = mutable.ListBuffer.empty[Either[Failure, Unit]]
    val nothing = Tree.Map.empty(None, map.location)
    var rest = List.empty[Tree.Map]
    val done = map.steps.foldLeft(map.copy(steps = Vector.empty)) { (current, step) =>
      step match {
        case Tree.Step.Injection(path, optional, at) =>
          // after a failure, into nothing, for its own errors alone
          val into = if (failures.isEmpty) current else Tree.Map.empty(None, at)
          val next = fields(id, into, path, optional, at) match {
            case Right(merged) if failures.isEmpty => merged
            case Right(_) => current
            case Left(failure) =>
              failures += Left(failure)
              current
          }
          // the fields after it, once one has failed, apart from those before it
          if (failures.nonEmpty) rest = nothing :: rest
          next
        case Tree.Step.Statements(statements) if failures.nonEmpty =>
          rest = merging.block(rest.head, statements) :: rest.tail
          current
        case _ if failures.nonEmpty => current
        case Tree.Step.Statements(statements) => merging.block(current, statements)
        case Tree.Step.Graft(content) => over(current, content)
      }
    }
    if (failures.nonEmpty) abandoned(id) = rest
    failure(failures.toList).toLeft(done)
  }

  /** The map at `id` once `completion` has completed it, its children resolved; failed, with the fields kept apart in
    * `abandoned` resolved for their own errors.
    */
  private def finished(id: Id, completion: Either[Failure, Tree.Map]): Resolution =
    completion match {
      case Right(done) => this.map(id, done).map(Some(_))
      case Left(failure) =>
        abandoned.remove(id).foreach(_.foreach(fields => this.map(id, fields)))
        Left(failure)
    }

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
      case found: Tree.Map => into(id, current, found, at, path)
      case other => failed(InjectionNotMap(at, Message.path(path), other.location))
    })

  /** The map `current` at `id` with the fields of `found`, the map at `path`, merged into it, as an injection at `at`
    * merges them.
    */
  private def into(id: Id, current: Tree.Map, found: Tree.Map, at: SourceLocation, path: Id.Branch)
      : Either[Failure, Tree.Map] =
    // its fields stand where the map's do
    if (segments(id).size - 1 + height(found) > limit) failed(DepthExceeded(at, limit, fatal = false))
    else
      grow(size(found) - 1, 0, ResolutionTooLarge(at, Message.path(path), nodeLimit))
        // a map grafted is a map
        .map(_ => over(current, graft(found, at, path, None).asInstanceOf[Tree.Map]))

  /** The elements of `found`, the sequence at `path`, injected at `at` where `around` levels stand around them. */
  private def elements(around: Int, found: Tree.Sequence, at: SourceLocation, path: Id.Branch)
      : Either[Failure, Vector[Tree.Value]] =
    if (around - 1 + height(found) > limit) failed(DepthExceeded(at, limit, fatal = false))
    else
      grow(size(found) - 1, 0, ResolutionTooLarge(at, Message.path(path), nodeLimit))
        // a sequence grafted is a sequence
        .map(_ => graft(found, at, path, None).asInstanceOf[Tree.Sequence].elements)

  /** What a block of injections alone at `id` gives (§4, §9), as [[read]] reads it, resolved. */
  private def gathered(id: Id, injections: Tree.Injections): Resolution =
    read(injections).flatMap {
      case None => Right(None)
      case Some(Left(map)) => step(finished(id, complete(id, map)))
      case Some(Right(sequence)) => step(this.sequence(id, sequence).map(Some(_)))
    }

  /** A block of injections alone read as a map or a sequence, still to be resolved (§4, §9): what the first that finds
    * or gives anything, as written, finds or gives decides, and the block is then read as written with a map's fields
    * or a sequence's elements: its substitutions injections among fields or among `- ` elements, and what its calls
    * gave put there, each of which must be the same. The block's tag is its own; none when nothing finds or gives
    * anything. Every substitution is resolved first, for its own errors.
    */
  private def read(injections: Tree.Injections): Either[Failure, Option[Either[Tree.Map, Tree.Sequence]]] = {
    val Tree.Injections(items, tag, location) = injections
    val found = items.map(_.map(r => r -> reference(r.path, r.optional, r.location)))
    val first = found.collectFirst {
      case Left(AST.Injected.Given(call, value)) => Left(call -> value)
      case Right((r, Right(Some(value)))) => Right(r -> value)
    }
    failure(found.collect { case Right((_, resolution)) => resolution }).toLeft(first).flatMap {
      case None => Right(None)
      case Some(Left((_, _: AST.Block)) | Right((_, _: Tree.Map))) =>
        val statements = items.flatMap {
          case Right(r) => Vector(AST.InjectionStatement(r.path, r.optional, r.location, None))
          case Left(AST.Injected.Given(_, AST.Block(statements, _, _))) => statements
          case Left(_) => Vector.empty
        }
        mismatched(items)(_.isInstanceOf[AST.Block], DirectiveNotMap.apply)
          .toLeft(Some(Left(merging.block(Tree.Map.empty(tag, location), statements))))
      case Some(Left((_, _: AST.Sequence)) | Right((_, _: Tree.Sequence))) =>
        val elements = items.flatMap {
          case Right(r) =>
            Vector(AST.Element(AST.Substitution(r.path, r.optional, None, r.location), None, AST.Put.Elements))
          case Left(AST.Injected.Given(_, AST.Sequence(elements, _, _))) => elements
          case Left(_) => Vector.empty
        }
        mismatched(items)(_.isInstanceOf[AST.Sequence], DirectiveNotSequence.apply)
          .toLeft(Some(Right(merging.sequence(elements, tag, location))))
      case Some(Left((call, _))) => failed(DirectiveNotContainer(call.location, call.name))
      case Some(Right((r, other))) => failed(InjectionNotContainer(r.location, Message.path(r.path), other.location))
    }
  }

  /** The failure of a block of injections alone whose calls gave what `fits` does not take, each reported as `error` at
    * its call; none when every one fits.
    */
  private def mismatched(items: Vector[Either[AST.Injected.Given, Token.Reference]])(
      fits: AST.Value => Boolean,
      error: (SourceLocation, String) => PsiError): Option[Failure] =
    failure(items.collect {
      case Left(AST.Injected.Given(call, value)) if !fits(value) => failed(error(call.location, call.name))
    })

  /** `value`, found by `\${path}` at `at`, grafted at `id` with the tag written before it in place of its own. */
  private def grafted(id: Id, value: Tree.Value, at: SourceLocation, path: Id.Branch, tag: Option[String]): Resolution =
    placed(id, value, at, path, tag).map(Some(_))

  /** `value` grafted at `id`, as [[grafted]] does. */
  private def placed(id: Id, value: Tree.Value, at: SourceLocation, path: Id.Branch, tag: Option[String])
      : Either[Failure, Tree.Value] =
    // the levels around the substitution, and those its value opens
    if (segments(id).size - 1 + height(value) > limit) failed(DepthExceeded(at, limit, fatal = false))
    else grow(size(value), 0, ResolutionTooLarge(at, Message.path(path), nodeLimit)).map(_ =>
      graft(value, at, path, tag))

  /** What an injection among elements finds: a sequence, whose elements are to stand in its place; none when an
    * optional one finds nothing. Anything else is an error for a `required` one, and stands there as one element
    * otherwise, with the tag written before it in place of its own, which a sequence's elements have no place for.
    */
  private def injected(id: Id, injection: Tree.Injection): Resolution = {
    val Tree.Injection(path, optional, tag, at, required) = injection
    reference(path, optional, at).flatMap(_.fold[Resolution](Right(None)) {
      case found: Tree.Sequence if tag.nonEmpty => failed(TaggedSequence(at, Message.path(path), found.location))
      case found: Tree.Sequence =>
        // its elements stand where the injection does, below the sequence it stands in
        elements(segments(id).size - 1, found, at, path).map(e => Some(found.copy(elements = e)))
      case found if required => failed(InjectionNotSequence(at, Message.path(path), found.location))
      case found => grafted(id, found, at, path, tag)
    })
  }

  /** `current` with the fields of `content` merged into it as `:` merges them (§7): maps merge, anything else replaces,
    * and where either may turn out a map or not, they are kept as layers, merged so once resolved.
    */
  private def over(current: Tree.Map, content: Tree.Map): Tree.Map =
    content.keys.foldLeft(current) { (map, key) =>
      val value = content.entries(key)
      map.updated(key, map.entries.get(key).fold(value)(combined(_, value)))
    }

  private def combined(earlier: Tree.Value, value: Tree.Value): Tree.Value =
    (earlier, value) match {
      case (map: Tree.Map, content: Tree.Map) => joined(map, content)
      case (earlier: Tree.Layer, value: Tree.Layer) => Tree.Layers(earlier, value)
      case _ => value
    }

  /** `content` merged over `map` with `:`, its tag if written, `map`'s otherwise: after the injections of `map`, when
    * it has any, and with those of `content` after it.
    */
  private def joined(map: Tree.Map, content: Tree.Map): Tree.Map = {
    val written = content.copy(steps = Vector.empty)
    val tagged = map.copy(tag = content.tag.orElse(map.tag), location = Merge.again(content.location, map.location))
    if (map.steps.isEmpty) over(tagged, written).copy(steps = content.steps)
    else tagged.copy(steps = tagged.steps ++ (Tree.Step.Graft(written) +: content.steps))
  }

  /** What layers merged with `:` at `id` end up as (§7). They are read from the last back to the first that is no map,
    * which the maps after it replace, or which stands alone when none comes after it; the maps after it are then merged
    * in turn as written, those a substitution finds with their tag. A layer that finds nothing puts nothing, and what
    * stands before the first that is no map is not resolved, as what a value replaces is not.
    */
  private def layered(id: Id, layers: Tree.Layers): Resolution = {
    // the first that is no map from the last back, if any, to resolve when nothing comes after it, and the maps after it
    @tailrec
    def back(rest: List[Tree.Layer], maps: List[Tree.Map]): Either[Failure, (() => Resolution, List[Tree.Map])] =
      rest match {
        case Nil => Right((() => Right(None), maps))
        case (map: Tree.Map) :: earlier => back(earlier, map :: maps)
        case Tree.Substitution(path, optional, tag, at) :: earlier =>
          reference(path, optional, at) match {
            case Right(None) => back(earlier, maps)
            case Right(Some(found: Tree.Map)) =>
              placed(id, found, at, path, tag) match {
                // a map grafted is a map
                case Right(map) => back(earlier, map.asInstanceOf[Tree.Map] :: maps)
                case Left(failure) => Left(failure)
              }
            case Right(Some(found)) => Right((() => placed(id, found, at, path, tag).map(Some(_)), maps))
            case Left(failure) => Left(failure)
          }
        case (injections: Tree.Injections) :: earlier =>
          read(injections) match {
            case Right(None) => back(earlier, maps)
            case Right(Some(Left(map))) => back(earlier, map :: maps)
            case Right(Some(Right(sequence))) => Right((() => step(this.sequence(id, sequence).map(Some(_))), maps))
            case Left(failure) => Left(failure)
          }
        // layers merged into a map in their turn, as they would have been at this key
        case Tree.Layers(inner) :: earlier => back(inner.reverse.toList ++ earlier, maps)
      }
    back(layers.layers.reverse.toList, Nil).flatMap {
      case (alone, Nil) => alone()
      case (_, first :: rest) => step(finished(id, complete(id, rest.foldLeft(first)(joined))))
    }
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
          case _: Tree.Substitution | _: Tree.Injections | _: Tree.Layers | _: Tree.Concatenation | _: Tree.Marked =>
            resolve(id, value)
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

  /** The child of `container` at `id` that `segment` names, with its position in the tree. An injection or an optional
    * substitution before it in a sequence is resolved, to know how many elements it puts, and an injection that puts
    * several gives its own position to each.
    */
  private def child(id: Id, container: Tree.Value, segment: Id.Branch): Either[Failure, Option[(Id, Tree.Value)]] =
    (container, segment) match {
      case (map: Tree.Map, Id.Key(key, _)) =>
        map.entries.get(key) match {
          case Some(value) => Right(Some(Id.Key(key, id) -> value))
          case None => Right(None)
        }
      case (sequence: Tree.Sequence, Id.Index(index, _)) =>
        @tailrec def element(i: Int, left: Int): Either[Failure, Option[(Id, Tree.Value)]] =
          if (i == sequence.elements.size) Right(None)
          else
            sequence.elements(i) match {
              case injection: Tree.Injection =>
                resolve(Id.Index(i, id), injection) match {
                  case Right(found) =>
                    // a sequence, anything else as one element, or nothing for an optional one
                    val elements = found.fold(Vector.empty[Tree.Value]) {
                      case put: Tree.Sequence => put.elements
                      case one => Vector(one)
                    }
                    if (left < elements.size) Right(Some(Id.Index(i, id) -> elements(left)))
                    else element(i + 1, left - elements.size)
                  case Left(failure) => Left(failure)
                }
              // one that finds nothing puts no element
              case value @ (Tree.Substitution(_, true, _, _) | _: Tree.Injections) =>
                resolve(Id.Index(i, id), value) match {
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
        Tree.Scalar(texts.mkString, concatenation.tag, location)
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

  /** `value` grafted at `at` by `\${path}`, with the tag written before it in place of its own (§9, §10): every node of
    * it keeps its origin under a reference to the path it was taken by.
    */
  private[psi] def graft(value: Tree.Value, at: SourceLocation, path: Id.Branch, tag: Option[String]): Tree.Value = {
    def moved(value: Tree.Value, id: Id.Branch, tag: Option[String]): Tree.Value = {
      val location = ReferenceLocation(at, Message.path(id), value.location)
      value match {
        case scalar: Tree.Scalar => scalar.copy(tag = tag.orElse(scalar.tag), location = location)
        case node: Tree.Null => node.copy(tag = tag.orElse(node.tag), location = location)
        case sequence: Tree.Sequence =>
          val elements = sequence.elements.zipWithIndex.map { case (e, i) => moved(e, Id.Index(i, id), None) }
          sequence.copy(elements = elements, tag = tag.orElse(sequence.tag), location = location)
        case map: Tree.Map =>
          val entries = map.entries.map { case (key, child) => key -> moved(child, Id.Key(key, id), None) }
          map.copy(entries = entries, tag = tag.orElse(map.tag), location = location)
        // a node is resolved whole before it is grafted
        case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Marked | _: Tree.Injection | _: Tree.Injections |
            _: Tree.Layers =>
          throw new IllegalStateException("An unresolved substitution at " + value.location.description + " grafted")
      }
    }
    moved(value, path, tag)
  }
}
