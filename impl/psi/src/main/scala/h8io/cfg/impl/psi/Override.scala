package h8io.cfg.impl.psi

import java.util.Locale

/** Overrides the values of a merged tree from the system properties and the environment (`format.md` §8, §9): what is
  * set at launch wins over what the sources say.
  *
  * Only a value the sources define is overridden, and only one that is not a container, at a path of keys: a scalar,
  * `null`, a substitution or a concatenation. A system property is named by the path's keys joined by dots, an
  * environment variable by [[env]]; a property wins over a variable. The new value is a string with the tags written in
  * the source and the location of the property or the variable.
  */
private[psi] object Override {
  def apply(root: Tree.Map, properties: Map[String, String], environment: Map[String, String]): Tree.Map = {
    def overridden(map: Tree.Map, path: List[String]): Tree.Map =
      map.keys.foldLeft(map) { (result, key) =>
        val keys = path :+ key
        result.entries(key) match {
          case nested: Tree.Map => result.updated(key, overridden(nested, keys))
          case _: Tree.Sequence => result
          case value => launched(keys, value.tags).fold(result)(result.updated(key, _))
        }
      }
    def launched(keys: List[String], tags: Set[String]): Option[Tree.Value] = {
      val property = keys.mkString(".")
      properties.get(property) match {
        case Some(value) => Some(Tree.Scalar(value, tags, SystemPropertyLocation(property)))
        case None =>
          val variable = env(keys)
          environment.get(variable).map(Tree.Scalar(_, tags, EnvLocation(variable)))
      }
    }
    overridden(root, Nil)
  }

  /** The environment variable a path of `keys` names: the keys in upper case, a `-` as `_`, joined by `_`. */
  def env(keys: List[String]): String = keys.map(_.toUpperCase(Locale.ROOT).replace('-', '_')).mkString("_")
}
