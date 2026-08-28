package h8io.cfg.impl

import h8io.cfg.{Id, Node}
import org.snakeyaml.engine.v2.api.lowlevel.{Present, Serialize}
import org.snakeyaml.engine.v2.api.{DumpSettings, LoadSettings}
import org.snakeyaml.engine.v2.common.FlowStyle
import org.snakeyaml.engine.v2.exceptions.{Mark, YamlEngineException}
import org.snakeyaml.engine.v2.nodes.{MappingNode, Node as YamlNode, NodeTuple, ScalarNode, SequenceNode, Tag}
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader
import org.snakeyaml.engine.v2.schema.{CoreSchema, Schema}

import java.io.Reader
import java.util.{Collections, List as JList, Optional}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

package object yaml {

  /** Key of the node property under which [[TaggedComposer]] stores the tag written in the source. */
  private[yaml] def TagProperty: String = "h8io.cfg.yaml.tag"

  /** The non-specific tag `!`. It carries no type information, so it is treated as no tag at all. */
  private[yaml] def NonSpecificTag: String = "!"

  /** YAML 1.2 core schema: `~`, `Null` and `NULL` are nulls and `<<` merge keys are supported. */
  private[yaml] val CoreSchemaInstance: Schema = new CoreSchema

  private[yaml] val DumpOptions: DumpSettings = DumpSettings.builder().build()

  private[yaml] def settings(label: String): LoadSettings =
    LoadSettings.builder().setLabel(label).setSchema(CoreSchemaInstance).build()

  /** Composes a single YAML document, returning an empty mapping for an empty input. */
  private[yaml] def compose(settings: LoadSettings, reader: Reader): YamlNode =
    new TaggedComposer(settings, new ParserImpl(settings, new StreamReader(settings, reader)))
      .getSingleNode
      .orElse(emptyMapping)

  private[yaml] def emptyMapping: MappingNode =
    new MappingNode(
      Tag.MAP,
      true,
      Collections.emptyList[NodeTuple],
      FlowStyle.BLOCK,
      Optional.empty[Mark],
      Optional.empty[Mark])

  /** The tag written in the source, or `None` when the tag was inferred by the resolver. */
  @inline private[yaml] def tagOf(node: YamlNode): Option[String] =
    Option(node.getProperty(TagProperty).asInstanceOf[String])

  @inline private[yaml] def keyOf(tuple: NodeTuple): String = tuple.getKeyNode.asInstanceOf[ScalarNode].getValue

  @inline private[yaml] def wrap[I <: Id](id: I, node: YamlNode): Node.ISome[I] =
    node match {
      case mapping: MappingNode => wrap(id, mapping)
      case sequence: SequenceNode => SeqImpl(id, sequence)
      case scalar: ScalarNode if scalar.getTag == Tag.NULL => Node.Null(id, tagOf(scalar), LocationImpl(scalar))
      case scalar: ScalarNode => Node.Scalar(id, tagOf(scalar), scalar.getValue, LocationImpl(scalar))
      case _ =>
        throw new YamlEngineException(
          s"Unexpected ${node.getNodeType} node at ${LocationImpl(node).description}")
    }

  @inline private[yaml] def wrap[I <: Id](id: I, mapping: MappingNode): Node.IMap[I] = MapImpl(id, mapping)

  /** The entries of a mapping in source order, with duplicate keys collapsed to their last occurrence. */
  private[yaml] def entriesOf(mapping: MappingNode): Vector[(String, NodeTuple)] = {
    val seen = mutable.Set.empty[String]
    mapping.getValue.asScala.reverseIterator
      .map(tuple => keyOf(tuple) -> tuple)
      .filter(entry => seen.add(entry._1))
      .toVector
      .reverse
  }

  /** A mapping with the tag, style and location of `source` but a different set of entries. */
  private[yaml] def copyOf(source: MappingNode, tuples: JList[NodeTuple]): MappingNode = {
    val result =
      new MappingNode(source.getTag, true, tuples, source.getFlowStyle, source.getStartMark, source.getEndMark)
    tagOf(source).foreach(tag => result.setProperty(TagProperty, tag))
    result
  }

  /** Overlays `next` onto `previous`: mappings are merged key by key, anything else is replaced outright. */
  private[yaml] def merge(previous: YamlNode, next: YamlNode): YamlNode =
    (previous, next) match {
      case (base: MappingNode, overlay: MappingNode) =>
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
      case _ => next
    }

  private[yaml] def render(node: YamlNode): String =
    new Present(DumpOptions).emitToString(new Serialize(DumpOptions).serializeOne(node).iterator)
}
