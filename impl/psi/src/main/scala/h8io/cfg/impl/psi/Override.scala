package h8io.cfg.impl.psi

/** Overrides the values of a merged tree from the system properties (`format.md` §8, §9): what is set at launch wins
  * over what the sources say.
  *
  * Only a value the sources define is overridden, and only one that is not a container, at a path of keys: a scalar,
  * `null`, a substitution or a concatenation, by the property named by the path's keys joined by dots. The new value is
  * a string with the tags written in the source and the location of the property.
  */
private[psi] object Override {
  def apply(root: Tree.Map, properties: Map[String, String]): Tree.Map = {
    def overridden(map: Tree.Map, path: List[String]): Tree.Map =
      map.keys.foldLeft(map) { (result, key) =>
        val keys = path :+ key
        result.entries(key) match {
          case nested: Tree.Map => result.updated(key, overridden(nested, keys))
          case _: Tree.Sequence => result
          case value =>
            val property = keys.mkString(".")
            properties.get(property).fold(result) { launched =>
              result.updated(key, Tree.Scalar(launched, value.tags, SystemPropertyLocation(property)))
            }
        }
      }
    overridden(root, Nil)
  }
}
