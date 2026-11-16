package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.nodes.{MappingNode, Node as YamlNode, NodeTuple}

import java.util.IdentityHashMap
import scala.jdk.CollectionConverters.*

/** One overlay of a mapping onto another. A pair of mappings is merged once, and its result shared wherever the pair
  * meets again: aliases make a tree of a file share nodes, and merging every path apart would copy each shared node as
  * many times as there are paths to it, exponentially many.
  */
private[yaml] final class Merging {
  private val merged = new IdentityHashMap[MappingNode, IdentityHashMap[MappingNode, MappingNode]]

  def merge(previous: YamlNode, next: YamlNode): YamlNode =
    (previous, next) match {
      case (base: MappingNode, overlay: MappingNode) => mappings(base, overlay)
      case _ => next
    }

  def mappings(base: MappingNode, overlay: MappingNode): MappingNode = {
    val byOverlay = merged.computeIfAbsent(base, _ => new IdentityHashMap[MappingNode, MappingNode])
    Option(byOverlay.get(overlay)).getOrElse {
      val result = overlaid(base, overlay)
      byOverlay.put(overlay, result)
      result
    }
  }

  private def overlaid(base: MappingNode, overlay: MappingNode): MappingNode = {
    val original = entriesOf(base)
    val overrides = entriesOf(overlay)
    val index = overrides.toMap
    val defined = original.iterator.map(_._1).toSet
    val updated = original.map { case (key, tuple) =>
      index.get(key).fold(tuple) { other =>
        new NodeTuple(other.getKeyNode, merge(tuple.getValueNode, other.getValueNode))
      }
    }
    val added = overrides.collect { case (key, tuple) if !defined(key) => tuple }
    copyOf(overlay, (updated ++ added).asJava)
  }
}
