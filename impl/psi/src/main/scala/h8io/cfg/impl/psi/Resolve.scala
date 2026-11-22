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
private[psi] final class Resolve(root: Tree.Map, limit: Int, nodeLimit: Int, characterLimit: Int, built: Long) {
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

  /** The positions `once` has begun resolving since a group of abandoned fields began, when one is being resolved. */
  private var marked: Option[mutable.ListBuffer[Id]] = None

  /** The nodes grafts have copied, those of the fragments handlers built among them, counted before resolution (§11);
    * and the characters of the scalars concatenations have built.
    */
  private var nodes = built
  private var characters = 0L

  /** Whether either has gone past its limit, after which nothing more is built. */
  private var exhausted = false

  /** Merges what injections bring into the maps they stand in, as it would have been merged had it been written there.
    */
  private val merging = new Merge

  /** By position, the fields written after an injection that failed, merged into nothing, those between two injections
    * apart, in the order written: resolved once the map has failed, for their own errors alone.
    */
  private val abandoned = mutable.Map.empty[Id, List[Tree.Map]]

  /** By position, the walk of the last sequence looked into there by index ([[indexed]]). */
  private val walks = mutable.Map.empty[Id, Walk]

  /** Whether the cycle the innermost substitution being followed is about to report was found walking a sequence to its
    * end for an index from the end, which [[reference]] reports as such and clears.
    */
  private var cycleFromEnd = false

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
      // merged once, its children resolved apart, after it, so that one may refer to another
      case contents: Tree.Contents => after(id, gathered(id, contents))
      case layers: Tree.Layers => after(id, stacked(id, layers))
      case Tree.Substitution(path, optional, tag, at) =>
        once(id, Some(new Following(at, path)))(
          reference(path, optional, at).flatMap(_.fold[Resolution](Right(None))(grafted(id, _, at, path, tag))))
      case concatenation: Tree.Concatenation =>
        once(id, first(concatenation))(this.concatenation(concatenation).map(Some(_)))
      case formatted: Tree.Formatted =>
        val reference = formatted.part.reference
        once(id, Some(new Following(reference.location, reference.path)))(this.formatted(formatted))
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
      text.flatMap(text => call.marker(call.arguments, text).left.map(MarkerFailed(call.location, call.name, _)))
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
    if (failures.nonEmpty) abandoned(id) = rest.reverse
    failure(failures.toList).toLeft(done)
  }

  /** The map at `id` once `completion` has completed it, its children resolved; failed, with the fields kept apart in
    * `abandoned` resolved for their own errors.
    */
  private def finished(id: Id, completion: Either[Failure, Tree.Map]): Resolution =
    completion match {
      case Right(done) => this.map(id, done).map(Some(_))
      case Left(failure) =>
        abandoned.remove(id).foreach(_.foreach(fields => apart(id, fields)))
        Left(failure)
    }

  /** Abandoned `fields` of the map at `id` resolved for their own errors, apart from the other groups: they stand at
    * the same positions, so what a group before resolved below `id` is forgotten after it, rather than taken for
    * theirs. What it resolved elsewhere stays, not to be counted again.
    */
  private def apart(id: Id, fields: Tree.Map): Unit = {
    val outer = marked
    val begun = mutable.ListBuffer.empty[Id]
    marked = Some(begun)
    try {
      val _ = this.map(id, fields)
    } finally marked = outer
    val (forgotten, kept) = begun.partition(below(_, id))
    resolved --= forgotten
    outer.foreach(_ ++= kept)
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

  /** What an injection after a key at `id` gives (§9), as [[read]] reads it, once: a map or a sequence with its
    * children not resolved yet.
    */
  private def gathered(id: Id, contents: Tree.Contents): Resolution =
    once(id, contents.item.toOption.map(r => new Following(r.location, r.path)))(
      read(contents).flatMap {
        case None => Right(None)
        case Some(Left(map)) => complete(id, map).map(Some(_))
        case Some(Right(sequence)) => Right(Some(sequence))
      })

  /** What `merged` gave at `id`, an injection after a key or layers, with its children resolved: apart from the merge,
    * after it, so that a child may refer to another, as a map's with injections do. A failed map's abandoned fields are
    * resolved for their own errors.
    */
  private def after(id: Id, merged: Resolution): Resolution =
    merged match {
      case Right(Some(map: Tree.Map)) => step(finished(id, Right(map)))
      case Right(Some(sequence: Tree.Sequence)) => step(this.sequence(id, sequence).map(Some(_)))
      case Left(failure) => finished(id, Left(failure))
      case other => other
    }

  /** An injection after a key read as a map or a sequence, still to be resolved (§9): a map holding the fields of the
    * map found or given, or a sequence holding the elements of the sequence, with the tag written before it and none of
    * the found node's own; none when an optional one finds nothing. Anything else is an error.
    */
  private def read(contents: Tree.Contents): Either[Failure, Option[Either[Tree.Map, Tree.Sequence]]] = {
    val Tree.Contents(item, tag, location) = contents
    item match {
      case Left(AST.Injected.Given(_, AST.Block(statements, _, _))) =>
        Right(Some(Left(merging.block(Tree.Map.empty(tag, location), statements))))
      case Left(AST.Injected.Given(_, AST.Sequence(elements, _, _))) =>
        Right(Some(Right(merging.sequence(elements, tag, location))))
      case Left(AST.Injected.Given(call, _)) => failed(DirectiveNotContainer(call.location, call.name))
      case Right(r) =>
        reference(r.path, r.optional, r.location).flatMap {
          case None => Right(None)
          case Some(_: Tree.Map) =>
            val injection = AST.InjectionStatement(r.path, r.optional, r.location, None)
            Right(Some(Left(merging.block(Tree.Map.empty(tag, location), Vector(injection)))))
          case Some(_: Tree.Sequence) =>
            val injection = AST.Substitution(r.path, r.optional, None, r.location)
            Right(Some(Right(merging.sequence(Vector(AST.Element(injection, None, AST.Put.Elements)), tag, location))))
          case Some(other) => failed(InjectionNotContainer(r.location, Message.path(r.path), other.location))
        }
    }
  }

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
    * optional one finds nothing. Anything else is an error.
    */
  private def injected(id: Id, injection: Tree.Injection): Resolution = {
    val Tree.Injection(path, optional, at) = injection
    reference(path, optional, at).flatMap(_.fold[Resolution](Right(None)) {
      case found: Tree.Sequence =>
        // its elements stand where the injection does, below the sequence it stands in
        elements(segments(id).size - 1, found, at, path).map(e => Some(found.copy(elements = e)))
      case found => failed(InjectionNotSequence(at, Message.path(path), found.location))
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
  /** What `layers` at `id` merge to, once: a map or a sequence with its children not resolved yet, or anything else
    * resolved.
    */
  private def stacked(id: Id, layers: Tree.Layers): Resolution =
    once(
      id,
      layers.layers.collectFirst {
        case Tree.Substitution(path, _, _, at) => new Following(at, path)
        case Tree.Formatted(Token.Formatted(Token.Reference(path, _, at), _), _, _) => new Following(at, path)
      }
    )(layered(id, layers))

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
        case (contents: Tree.Contents) :: earlier =>
          read(contents) match {
            case Right(None) => back(earlier, maps)
            case Right(Some(Left(map))) => back(earlier, map :: maps)
            case Right(Some(Right(sequence))) => Right((() => Right(Some(sequence)), maps))
            case Left(failure) => Left(failure)
          }
        // a formatted substitution gives a scalar, or nothing when an optional one finds nothing
        case (formatted: Tree.Formatted) :: earlier =>
          this.formatted(formatted) match {
            case Right(None) => back(earlier, maps)
            case Right(found) => Right((() => Right(found), maps))
            case Left(failure) => Left(failure)
          }
        // layers merged into a map in their turn, as they would have been at this key
        case Tree.Layers(inner) :: earlier => back(inner.reverse.toList ++ earlier, maps)
      }
    back(layers.layers.reverse.toList, Nil).flatMap {
      case (alone, Nil) => alone()
      case (_, first :: rest) => complete(id, rest.foldLeft(first)(joined)).map(Some(_))
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
        marked.foreach(_ += id)
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
        case Left(Cycle) if cycleFromEnd =>
          cycleFromEnd = false
          failed(CycleFromEnd(at, Message.path(path)))
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
          case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Formatted | _: Tree.Marked =>
            resolve(id, value)
          // what they merge to, its children looked into one at a time
          case contents: Tree.Contents => gathered(id, contents)
          case layers: Tree.Layers => stacked(id, layers)
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
      case (sequence: Tree.Sequence, Id.Index(index, _)) => indexed(id, sequence, index)
      case _ => Right(None)
    }

  /** The element of `sequence` at `id` that `index` names. An injection or an optional substitution before it is
    * resolved, to know how many elements it puts, and an injection that puts several gives its own position to each.
    *
    * Walking from the first element on every lookup took time linear in the index, and a sequence looked into at every
    * index quadratic: the walk is kept, what it found and how deep resolving what went before each went, and a lookup
    * takes it up where the last one stopped. One that finds its element already found counts as deep as walking to it
    * would, and fails where that would go past the limit, at the first element resolved before it that resolving again
    * goes past, as the walk would. A lookup into another sequence at `id`, which an injection after a key gives anew
    * each time, starts the walk anew.
    */
  private def indexed(id: Id, sequence: Tree.Sequence, index: Int): Either[Failure, Option[(Id, Tree.Value)]] = {
    val walk = walks.get(id).filter(_.sequence eq sequence).getOrElse {
      val walk = new Walk(sequence)
      walks(id) = walk
      walk
    }
    // an index from the end walks to the end, and counts as deep as that
    val height = if (index >= 0 && index < walk.found.size) walk.heights(index) else walk.height
    if (height >= 0 && steps + height > limit) {
      // resolved before, and so once more past the limit, the failure reported as resolving it again reports it
      val (first, _) = walk.resolved.find { case (_, height) => steps + height > limit }.get
      Left(resolve(Id.Index(first, id), sequence.elements(first)).swap.toOption.get)
    } else {
      deepest = math.max(deepest, steps + height)
      if (index >= 0) extended(id, walk, index)
      else
        extended(id, walk, Int.MaxValue) match {
          case Left(Cycle) =>
            // an element after the one it names leads back to it, or it is itself one of them
            cycleFromEnd = true
            Left(Cycle)
          case Left(failure) => Left(failure)
          case Right(_) =>
            val position = walk.found.size + index
            Right(if (position >= 0) Some(walk.found(position)) else None)
        }
    }
  }

  /** The element at `index` of `walk`'s sequence, at `id`, walking it on from where it stopped as far as `index`. */
  @tailrec private def extended(id: Id, walk: Walk, index: Int): Either[Failure, Option[(Id, Tree.Value)]] =
    if (index < walk.found.size) Right(Some(walk.found(index)))
    else if (walk.scanned == walk.sequence.elements.size) Right(None)
    else {
      val position = Id.Index(walk.scanned, id)
      walk.sequence.elements(walk.scanned) match {
        // what may put other than one element: an injection, and an optional substitution, formatted or not, which may
        // put none; a required formatted one is one element, a string, and resolving it to count would be a cycle
        // where it refers into its own sequence
        case value @ (_: Tree.Injection | Tree.Substitution(_, true, _, _) |
            Tree.Formatted(Token.Formatted(Token.Reference(_, true, _), _), _, _)) =>
          val outer = deepest
          deepest = steps
          val resolution = resolve(position, value)
          val height = deepest - steps
          deepest = math.max(outer, deepest)
          walk.resolved += walk.scanned -> height
          resolution match {
            case Right(found) =>
              // a lookup made while resolving it never walks past it: it would resolve it again, a cycle
              walk.add(
                position,
                value match {
                  // the elements of the sequence it found, or nothing for an optional one finding nothing
                  case _: Tree.Injection =>
                    found.collect { case put: Tree.Sequence => put.elements }.getOrElse(Vector.empty)
                  // one that finds nothing puts no element, a formatted one as any other
                  case _ => found.toVector
                },
                height
              )
              extended(id, walk, index)
            case Left(failure) => Left(failure)
          }
        case value =>
          walk.add(position, Vector(value), -1)
          extended(id, walk, index)
      }
    }

  /** A scalar of text and `\${…}`, each referring to a scalar; an optional one with nothing to refer to is empty. */
  private def concatenation(concatenation: Tree.Concatenation): Either[Failure, Tree.Scalar] = {
    val parts = concatenation.parts.map {
      case Token.Text(text) => Right(Vector(text) -> None)
      case part: Token.Reference =>
        val (path, optional, at) = (part.path, part.optional, part.location)
        reference(path, optional, at).flatMap {
          case Some(Tree.Scalar(text, _, origin)) =>
            Right(Vector(text) -> Some(ReferenceLocation(at, Message.path(path), origin)))
          case Some(other) => failed(NonScalarInConcatenation(at, Message.path(path), other.location))
          case None => Right(Vector.empty -> None)
        }
      case Token.Formatted(Token.Reference(path, optional, at), formatting) =>
        reference(path, optional, at).flatMap {
          case Some(found) =>
            format(formatting, found, path).map(_ -> Some(ReferenceLocation(at, Message.path(path), found.location)))
          case None => Right(Vector.empty -> None)
        }
    }
    failure(parts).toLeft(parts.collect { case Right(part) => part }).flatMap { parts =>
      val (texts, references) = parts.unzip
      built(texts.flatten, counted = true, concatenation.tag, concatenation.location, references.flatten.toList)
    }
  }

  /** A formatted substitution alone (§9): the text its formatter gives, a scalar with the tag written before it, and
    * nothing when an optional one finds nothing. What a loader's formatter gives is not counted, being a handler's, and
    * what `join` builds is (§11).
    */
  private def formatted(formatted: Tree.Formatted): Resolution = {
    val Tree.Formatted(Token.Formatted(Token.Reference(path, optional, at), formatting), tag, location) = formatted
    reference(path, optional, at).flatMap {
      case Some(found) =>
        format(formatting, found, path).flatMap { texts =>
          val counted = formatting.isInstanceOf[Token.Join]
          built(texts, counted, tag, location, ReferenceLocation(at, Message.path(path), found.location) :: Nil)
            .map(Some(_))
        }
      case None => Right(None)
    }
  }

  /** A scalar of `texts`, joined once their characters are `counted` against the limit when they are, located at
    * `location` with the references that gave them.
    */
  private def built(
      texts: Vector[String],
      counted: Boolean,
      tag: Option[String],
      location: SourceLocation,
      references: List[ReferenceLocation]): Either[Failure, Tree.Scalar] =
    grow(0, if (counted) texts.foldLeft(0L)(_ + _.length) else 0L, ConcatenationTooLong(location, characterLimit)).map {
      _ =>
        val at = references match {
          case Nil => location
          case first :: rest => ConcatenationLocation(location, ::(first, rest))
        }
        Tree.Scalar(texts.mkString, tag, at)
    }

  /** The text `formatting` gives for `found`, the node at `path`, in pieces not joined yet, so that what they add is
    * counted before it is built (§11); an error of the formatter is reported at its name.
    */
  private def format(formatting: Token.Formatting, found: Tree.Value, path: Id.Branch)
      : Either[Failure, Vector[String]] =
    formatting match {
      case Token.Join(separator, _, at) =>
        found match {
          case Tree.Sequence(elements, _, _) =>
            elements.zipWithIndex.collectFirst { case (element, i) if !element.isInstanceOf[Tree.Scalar] => i } match {
              case Some(i) =>
                failed(FormatterFailed(at, formatting.name, JoinElementNotScalar(i, elements(i).location)))
              case None =>
                Right(elements.collect { case Tree.Scalar(text, _, _) => Vector(separator, text) }.flatten.drop(1))
            }
          case other => failed(FormatterFailed(at, formatting.name, JoinNotSequence(other.location)))
        }
      case Token.Handled(name, _, at, format) =>
        format(wrap(path, found)).fold(error => failed(FormatterFailed(at, name, error)), text => Right(Vector(text)))
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

  /** Whether `at` is a position below `id`. */
  @tailrec private def below(at: Id, id: Id): Boolean =
    at match {
      case branch: Id.Branch => branch.parent == id || below(branch.parent, id)
      case _ => false
    }

  /** A walk of `sequence` by index: the elements found so far, each with its position, `scanned` the elements of the
    * sequence walked past, and `height` how many steps deeper than the walk resolving them went, -1 for none resolved;
    * `heights` holds that height as it was when each element found was, and `resolved` each element resolved, by index,
    * with how many steps deeper resolving it went.
    */
  private final class Walk(val sequence: Tree.Sequence) {
    val found: mutable.ArrayBuffer[(Id, Tree.Value)] = mutable.ArrayBuffer.empty
    val heights: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
    val resolved: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer.empty
    var scanned = 0
    var height: Int = -1

    /** Walks past the element at `position`, which put `elements`, resolving it `height` steps deep, -1 for none. */
    def add(position: Id, elements: Vector[Tree.Value], height: Int): Unit = {
      this.height = math.max(this.height, height)
      elements.foreach { element =>
        found += position -> element
        heights += this.height
      }
      scanned += 1
    }
  }

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
    concatenation.parts.collectFirst {
      case Token.Reference(path, _, at) => new Following(at, path)
      case Token.Formatted(Token.Reference(path, _, at), _) => new Following(at, path)
    }

  def apply(
      root: Tree.Map,
      limit: Int = PsiLoader.DefaultDepthLimit,
      nodeLimit: Int = PsiLoader.DefaultNodeLimit,
      characterLimit: Int = PsiLoader.DefaultCharacterLimit,
      built: Long = 0): Result[Tree.Map] =
    new Resolve(root, limit, nodeLimit, characterLimit, built).tree

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
        case _: Tree.Substitution | _: Tree.Concatenation | _: Tree.Formatted | _: Tree.Marked | _: Tree.Injection |
            _: Tree.Contents | _: Tree.Layers =>
          throw new IllegalStateException("An unresolved substitution at " + value.location.description + " grafted")
      }
    }
    moved(value, path, tag)
  }
}
