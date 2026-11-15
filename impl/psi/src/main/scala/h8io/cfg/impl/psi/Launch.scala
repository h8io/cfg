package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import scala.collection.mutable

/** Why `#prop` or `#env` failed (`format.md` §6), carried by the [[DirectiveFailed]] at the call. */
sealed trait LaunchError extends DirectiveError

/** No system property `name`, nor any whose name starts with `name.`, and neither `optional` nor `default` given. */
final case class PropertyNotSet(name: String) extends LaunchError {
  def message: String = "the system property " + Message.quote(name) + " is not set, nor any under it"

  override def hint: Option[String] = Some("set it at launch, or give the call optional: true or a default")
}

/** No environment variable `name`, and neither `optional` nor `default` given. */
final case class VariableNotSet(name: String) extends LaunchError {
  def message: String = "the environment variable " + Message.quote(name) + " is not set"

  override def hint: Option[String] = Some("set it at launch, or give the call optional: true or a default")
}

/** `optional` neither `true` nor `false`. */
final case class LaunchOptional(value: String) extends LaunchError {
  def message: String = "optional is true or false, not " + value
}

/** `optional` and `default` both given: a default makes the value always there. */
case object OptionalWithDefault extends LaunchError {
  def message: String = "optional and default cannot both be given"

  override def hint: Option[String] = Some("keep the default, which is used when the value is not set")
}

/** Among the system properties under the one asked for, `name` has a value and properties under it too, so that it
  * would be a scalar and a map at once.
  */
final case class PropertyConflict(name: String) extends LaunchError {
  def message: String = "the system property " + Message.quote(name) + " has a value and properties under it"
}

/** The built-in `#prop(name)` and `#env(name)` (`format.md` §6): the value of a system property or an environment
  * variable, read where the call stands; with `optional: true`, nothing when it is not set, and with `default: …`, the
  * default.
  */
private[psi] abstract class Launch(val name: String) extends Directive {
  def parameters: Seq[Parameter] =
    Seq(Parameter("name", required = true), Parameter("optional", required = false), Parameter("default", false))

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Option[Fragment]] = {
    def argument(name: String): Option[Node.IScalar[?]] =
      arguments(name) match {
        case scalar: Node.IScalar[?] => Some(scalar)
        case _ => None
      }
    // required, so always given: the framework calls the handler with every required argument
    val key = argument("name").map(_.value).mkString
    val default = argument("default")
    for {
      optional <- argument("optional").map(_.value) match {
        case None | Some("false") => Right(false)
        case Some("true") => Right(true)
        case Some(other) => Left(LaunchOptional(other))
      }
      _ <- if (optional && default.nonEmpty) Left(OptionalWithDefault) else Right(())
      found <- value(key)
      node <- found.orElse(default.map(d => Node.Scalar(Id.Root, Set.empty, d.value, d.location))) match {
        case Some(node) => Right(Some(Fragment(node)))
        case None => if (optional) Right(None) else Left(notSet(key))
      }
    } yield node
  }

  /** The value `key` names, none when it is not set. */
  protected def value(key: String): Either[LaunchError, Option[Node.ISome[Id]]]

  protected def notSet(key: String): LaunchError
}

private[psi] object Launch {

  /** `#prop(name)`: the system property `name`, or, when there is none, a map of every property whose name starts with
    * `name.`, the rest of the name split at its dots into keys: `#prop("java")` gives `java.version` as `version`.
    */
  final class Prop(properties: Map[String, String]) extends Launch("prop") {
    protected def notSet(key: String): LaunchError = PropertyNotSet(key)

    protected def value(key: String): Either[LaunchError, Option[Node.ISome[Id]]] =
      properties.get(key) match {
        case Some(value) => Right(Some(Node.Scalar(Id.Root, Set.empty, value, SystemPropertyLocation(key))))
        case None =>
          val prefix = key + "."
          val under = properties.keys.filter(_.startsWith(prefix)).toVector.sorted
          if (under.isEmpty) Right(None)
          else tree(key, under.map(name => name -> name.substring(prefix.length).split("\\.", -1).toList)).map {
            map => Some(MapImpl(Id.Root, map))
          }
      }

    /** The map of the properties `names`, each with the keys its name splits into below `key`; built and turned into a
      * tree with stacks of their own, a name having any number of dots.
      */
    private def tree(key: String, names: Vector[(String, List[String])]): Either[LaunchError, Tree.Map] = {
      final class Branch(val name: String) {
        val children = mutable.LinkedHashMap.empty[String, Either[String, Branch]]
      }
      val root = new Branch(key)
      // the property in the way of `name`: one whose name is a prefix of it, or the other way round
      def insert(name: String, keys: List[String]): Option[String] =
        keys.init.foldLeft[Either[String, Branch]](Right(root)) { (at, next) =>
          at.flatMap(branch => branch.children.getOrElseUpdate(next, Right(new Branch(branch.name + "." + next))))
        } match {
          case Left(leaf) => Some(leaf)
          // sorted, a name comes before every name it is a prefix of, so nothing stands at its last key yet
          case Right(branch) =>
            branch.children(keys.last) = Left(name)
            None
        }
      val conflict = names.iterator.map { case (name, keys) => insert(name, keys) }.collectFirst { case Some(c) => c }
      conflict.map(PropertyConflict(_)).toLeft {
        // every branch before those below it, so that each is built after its children, in reverse
        val order = mutable.ArrayBuffer(root)
        var i = 0
        while (i < order.length) {
          order(i).children.valuesIterator.foreach(_.foreach(order += _))
          i += 1
        }
        val built = new java.util.IdentityHashMap[Branch, Tree.Map]
        order.reverseIterator.foreach { branch =>
          val entries = branch.children.iterator.map {
            case (k, Left(name)) => k ->
                (Tree.Scalar(properties(name), Set.empty, SystemPropertyLocation(name)): Tree.Value)
            case (k, Right(child)) => k -> (built.get(child): Tree.Value)
          }.toVector
          val _ = built.put(
            branch,
            Tree.Map(entries.map(_._1), entries.toMap, Set.empty, SystemPropertyLocation(branch.name)))
        }
        built.get(root)
      }
    }
  }

  /** `#env(name)`: the environment variable `name`. */
  final class Env(environment: Map[String, String]) extends Launch("env") {
    protected def notSet(key: String): LaunchError = VariableNotSet(key)

    protected def value(key: String): Either[LaunchError, Option[Node.ISome[Id]]] =
      Right(environment.get(key).map(value => Node.Scalar(Id.Root, Set.empty, value, EnvironmentLocation(key))))
  }
}
