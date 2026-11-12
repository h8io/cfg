package h8io.cfg.impl.psi

import h8io.cfg.Id

import scala.annotation.tailrec
import scala.collection.mutable

/** Resolves the substitutions of a merged tree (`format.md` §9), giving a tree with none left.
  *
  * A path is looked up in the tree, then in the system properties, then in the environment, the first that has it
  * winning. A substitution on the way is resolved first, to look into what it grafts; the node found is resolved whole
  * before it is grafted. Each substitution of the tree is resolved once, by its position, and the position is marked
  * while it is being resolved: reaching it again is a cycle, reported by the reference that reached it.
  *
  * Errors are root causes only (§11): a node that depends on a failed one fails silently, and so does every container
  * holding it — the result is then discarded anyway.
  */
private[psi] final class Resolve(root: Tree.Map, properties: () => Map[String, String],
    env: () => Map[String, String]) {
  import Resolve.*

  private val errors = mutable.ListBuffer.empty[PsiError]

  /** By position, a substitution resolved, or `None` while it is being resolved. */
  private val resolved = mutable.Map.empty[Id, Option[Resolution]]

  private lazy val propertyTree = Resolve.properties(properties())

  private lazy val envTree = Resolve.environment(env())

  def tree: Result[Tree.Map] = Result(map(Id.Root, root).getOrElse(root), errors.toList)

  /** The value at `id` resolved: `Right(None)` when it is an optional substitution with nothing to graft. */
  private def resolve(id: Id, value: Tree.Value): Resolution =
    value match {
      case _: Tree.Scalar | _: Tree.Null => Right(Some(value))
      case map: Tree.Map => this.map(id, map).map(Some(_))
      case sequence: Tree.Sequence => this.sequence(id, sequence).map(Some(_))
      case Tree.Substitution(path, optional, tags, at) =>
        once(id)(reference(path, optional, at).map(_.map(graft(_, at, path, tags))))
      case concatenation: Tree.Concatenation => once(id)(this.concatenation(concatenation))
    }

  private def map(id: Id, map: Tree.Map): Either[Failure, Tree.Map] = {
    val entries = map.keys.map(key => key -> resolve(Id.Key(key, id), map.entries(key)))
    failure(entries.map(_._2)).toLeft {
      val present = entries.collect { case (key, Right(Some(value))) => key -> value }
      map.copy(keys = present.map(_._1), entries = present.toMap)
    }
  }

  private def sequence(id: Id, sequence: Tree.Sequence): Either[Failure, Tree.Sequence] = {
    val elements = sequence.elements.zipWithIndex.map { case (value, i) => resolve(Id.Index(i, id), value) }
    failure(elements).toLeft(sequence.copy(elements = elements.collect { case Right(Some(value)) => value }))
  }

  private def once(id: Id)(resolution: => Resolution): Resolution =
    resolved.get(id) match {
      case Some(Some(done)) => done
      case Some(None) => Left(Cycle)
      case None =>
        resolved(id) = None
        val done = resolution
        resolved(id) = Some(done)
        done
    }

  /** `\${path}` or `\${?path}` at `at`: the node it refers to, resolved, or `None` for an optional one with none. */
  private def reference(path: Id.Branch, optional: Boolean, at: SourceLocation): Resolution =
    lookup(path) match {
      case Left(Cycle) => failed(SubstitutionCycle(at, path.path))
      case Right(None) if !optional => failed(UnresolvedSubstitution(at, path.path))
      case other => other
    }

  private def failed[A](error: PsiError): Either[Failure, A] = {
    errors += error
    Left(Failed)
  }

  /** The tree, the system properties and the environment in turn; a failure in the tree is not looked past. */
  private def lookup(path: Id.Branch): Resolution = {
    val segments = Resolve.segments(path)
    find(Id.Root, root, segments) match {
      case Right(None) =>
        find(Id.Root, propertyTree, segments) match {
          case Right(None) => find(Id.Root, envTree, segments)
          case found => found
        }
      case other => other
    }
  }

  /** The node at `segments` below `value` at `id`, resolved. */
  private def find(id: Id, value: Tree.Value, segments: List[Id.Branch]): Resolution =
    segments match {
      case Nil => resolve(id, value)
      case next :: rest =>
        val here = value match {
          case _: Tree.Substitution | _: Tree.Concatenation => resolve(id, value)
          case _ => Right(Some(value))
        }
        here.flatMap(_.fold[Resolution](Right(None)) { container =>
          child(id, container, next).flatMap(_.fold[Resolution](Right(None)) { case (childId, child) =>
            find(childId, child, rest)
          })
        })
    }

  /** The child of `container` at `id` that `segment` names, with its position in the tree. An optional substitution
    * before it in a sequence is resolved, to know whether it drops out and moves the rest up.
    */
  private def child(id: Id, container: Tree.Value, segment: Id.Branch): Either[Failure, Option[(Id, Tree.Value)]] =
    (container, segment) match {
      case (map: Tree.Map, Id.Key(key, _)) => Right(map.entries.get(key).map(Id.Key(key, id) -> _))
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
  private def concatenation(concatenation: Tree.Concatenation): Resolution = {
    val parts = concatenation.parts.map {
      case Token.Text(text) => Right(text -> None)
      case Token.Reference(path, optional, at) =>
        reference(path, optional, at).flatMap {
          case Some(Tree.Scalar(text, _, origin)) => Right(text -> Some(ReferenceLocation(at, path.path, origin)))
          case Some(other) => failed(NonScalarInConcatenation(at, path.path, other.location))
          case None => Right("" -> None)
        }
    }
    failure(parts).toLeft {
      val (texts, references) = parts.collect { case Right(part) => part }.unzip
      val location = references.flatten.toList match {
        case Nil => concatenation.location
        case first :: rest => ConcatenationLocation(concatenation.location, ::(first, rest))
      }
      Some(Tree.Scalar(texts.mkString, concatenation.tags, location))
    }
  }
}

private[psi] object Resolve {

  /** Why a node did not resolve: it failed, its error already reported, or it is part of a cycle not reported yet. */
  sealed trait Failure

  case object Failed extends Failure

  case object Cycle extends Failure

  type Resolution = Either[Failure, Option[Tree.Value]]

  def apply(root: Tree.Map, properties: () => Map[String, String], env: () => Map[String, String]): Result[Tree.Map] =
    new Resolve(root, properties, env).tree

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
        case _: Tree.Substitution | _: Tree.Concatenation =>
          throw new IllegalStateException("An unresolved substitution at " + value.location.description + " grafted")
      }
    }
    moved(value, path, tags)
  }

  /** The system properties as a tree of strings, their names split at the dots: a name that is both a value and a
    * prefix of others, `a` with `a.b`, loses its value to the map (§9). Sorted, a prefix comes before the names it
    * prefixes, so a map is never found where a value is to go.
    */
  private[psi] def properties(properties: Map[String, String]): Tree.Map =
    properties.toVector.sortBy(_._1).foldLeft(Tree.Map.empty(Set.empty, NoSourceLocation)) {
      case (tree, (name, value)) =>
        def put(map: Tree.Map, path: String, key: String, rest: List[String]): Tree.Map =
          rest match {
            case Nil => map.updated(key, Tree.Scalar(value, Set.empty, SystemPropertyLocation(name)))
            case next :: more =>
              val nested = map.entries.get(key) match {
                case Some(nested: Tree.Map) => nested
                case _ => Tree.Map.empty(Set.empty, SystemPropertyLocation(path))
              }
              map.updated(key, put(nested, path + '.' + next, next, more))
          }
        val keys = name.split("\\.", -1).toList
        put(tree, keys.head, keys.head, keys.tail)
    }

  /** The environment as a map of strings, one key per variable, not split (§9). */
  private[psi] def environment(env: Map[String, String]): Tree.Map =
    env.toVector.sortBy(_._1).foldLeft(Tree.Map.empty(Set.empty, NoSourceLocation)) { case (tree, (name, value)) =>
      tree.updated(name, Tree.Scalar(value, Set.empty, EnvLocation(name)))
    }
}
