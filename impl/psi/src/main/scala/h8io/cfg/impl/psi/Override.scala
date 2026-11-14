package h8io.cfg.impl.psi

import h8io.cfg.Id

/** Overrides the values of a merged tree from the system properties (`format.md` §8, §9): what is set at launch wins
  * over what the sources say.
  *
  * Only a value the sources define is overridden, and only one that is not a container, at a path of keys: a scalar,
  * `null`, a substitution or a concatenation, by the property named by the path as `Id.path` writes it: its keys joined
  * by dots, one that is not bare in backticks, so that a key `a.b` and a key `b` in a block `a` are told apart. The new
  * value is a string with the tags written in the source and the location of the property.
  */
private[psi] object Override {
  def apply(root: Tree.Map, properties: Map[String, String]): Tree.Map = {
    def overridden(map: Tree.Map, id: Id): Tree.Map =
      map.keys.foldLeft(map) { (result, key) =>
        val child = Id.Key(key, id)
        result.entries(key) match {
          case nested: Tree.Map => result.updated(key, overridden(nested, child))
          case _: Tree.Sequence => result
          case value =>
            val property = Message.path(child)
            properties.get(property).fold(result) { launched =>
              result.updated(key, Tree.Scalar(launched, value.tags, SystemPropertyLocation(property)))
            }
        }
      }
    overridden(root, Id.Root)
  }
}
