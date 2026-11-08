package h8io.cfg.impl

import h8io.cfg.{Id, Node}
import org.snakeyaml.engine.v2.api.lowlevel.{Present, Serialize}
import org.snakeyaml.engine.v2.api.{DumpSettings, LoadSettings}
import org.snakeyaml.engine.v2.common.FlowStyle
import org.snakeyaml.engine.v2.composer.Composer
import org.snakeyaml.engine.v2.exceptions.{Mark, YamlEngineException}
import org.snakeyaml.engine.v2.nodes.{MappingNode, Node as YamlNode, NodeTuple, ScalarNode, SequenceNode, Tag}
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader
import org.snakeyaml.engine.v2.schema.{CoreSchema, Schema}

import java.io.Reader
import java.util.{Collections, IdentityHashMap, List as JList, Optional}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

package object yaml {

  /** YAML 1.2 core schema: `~`, `Null` and `NULL` are nulls and `<<` merge keys are supported. */
  private[yaml] val CoreSchemaInstance: Schema = new CoreSchema

  private[yaml] val DumpOptions: DumpSettings = DumpSettings.builder().build()

  private[yaml] def settings(label: String): LoadSettings =
    LoadSettings.builder().setLabel(label).setSchema(CoreSchemaInstance).build()

  /** Composes a single YAML document, returning an empty mapping for an empty input. */
  private[yaml] def compose(settings: LoadSettings, reader: Reader): YamlNode =
    validated(
      new Composer(settings, new ParserImpl(settings, new StreamReader(settings, reader)))
        .getSingleNode
        .orElse(emptyMapping))

  /** Rejects a node that an alias makes contain itself, which a config tree cannot hold and `merge` would recurse into
    * forever. An anchor reused anywhere outside its own subtree is fine, and each node is visited once however often it
    * is reused. Keys need no check: snakeyaml-engine refuses keys that are not scalars by default, recursive ones
    * included, which is also what makes `keyOf`'s cast safe.
    */
  private[yaml] def validated(root: YamlNode): YamlNode = {
    def identitySet = Collections.newSetFromMap(new IdentityHashMap[YamlNode, java.lang.Boolean]).asScala
    val visited = identitySet
    val inProgress = identitySet
    def visit(node: YamlNode): Unit =
      if (!visited(node)) {
        if (!inProgress.add(node))
          throw new YamlEngineException(
            s"An alias makes the ${node.getNodeType.toString} at ${LocationImpl(node).description} contain itself")
        node match {
          case mapping: MappingNode =>
            mapping.getValue.asScala.foreach(tuple => visit(tuple.getValueNode))
          case sequence: SequenceNode => sequence.getValue.asScala.foreach(visit)
          case _ =>
        }
        inProgress -= node
        visited += node
      }
    visit(root)
    root
  }

  private[yaml] def emptyMapping: MappingNode =
    new MappingNode(
      Tag.MAP,
      true,
      Collections.emptyList[NodeTuple],
      FlowStyle.BLOCK,
      Optional.empty[Mark],
      Optional.empty[Mark])

  /** The tag written in the source without its leading `!`, or none when the tag was inferred by the resolver or is the
    * non-specific `!`. YAML allows one tag per node, so the set holds at most one name.
    */
  @inline private[yaml] def tagOf(node: YamlNode): Set[String] =
    if (node.isResolved) Set.empty
    else {
      val tag = node.getTag.getValue
      Set(if (tag.startsWith("!")) tag.substring(1) else tag)
    }

  @inline private[yaml] def keyOf(tuple: NodeTuple): String = tuple.getKeyNode.asInstanceOf[ScalarNode].getValue

  @inline private[yaml] def wrap[I <: Id](id: I, node: YamlNode): Node.ISome[I] =
    node match {
      case mapping: MappingNode => wrap(id, mapping)
      case sequence: SequenceNode => SeqImpl(id, sequence)
      case scalar: ScalarNode if scalar.getTag == Tag.NULL => Node.Null(id, tagOf(scalar), LocationImpl(scalar))
      case scalar: ScalarNode => Node.Scalar(id, tagOf(scalar), scalar.getValue, LocationImpl(scalar))
      case _ =>
        throw new YamlEngineException(
          s"Unexpected ${node.getNodeType.toString} node at ${LocationImpl(node).description}")
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
  private[yaml] def copyOf(source: MappingNode, tuples: JList[NodeTuple]): MappingNode =
    new MappingNode(
      source.getTag,
      source.isResolved,
      tuples,
      source.getFlowStyle,
      source.getStartMark,
      source.getEndMark)

  /** Overlays `next` onto `previous`: mappings are merged key by key, anything else is replaced outright. */
  private[yaml] def merge(previous: YamlNode, next: YamlNode): YamlNode =
    (previous, next) match {
      case (base: MappingNode, overlay: MappingNode) => mergeMappings(base, overlay)
      case _ => next
    }

  /** Overlays the mapping `overlay` onto `base`, key by key. */
  private[yaml] def mergeMappings(base: MappingNode, overlay: MappingNode): MappingNode = {
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

  private[yaml] def render(node: YamlNode): String =
    new Present(DumpOptions).emitToString(new Serialize(DumpOptions).serializeOne(node).iterator)
}
