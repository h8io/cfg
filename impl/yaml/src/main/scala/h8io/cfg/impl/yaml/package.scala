package h8io.cfg.impl

import h8io.cfg.{Id, Location, Node}
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
import scala.jdk.CollectionConverters.*

package object yaml {

  /** YAML 1.2 core schema: `~`, `Null` and `NULL` are nulls and `<<` merge keys are supported. */
  private[yaml] val CoreSchemaInstance: Schema = new CoreSchema

  private[yaml] val DumpOptions: DumpSettings = DumpSettings.builder().build()

  private[yaml] def settings(label: String): LoadSettings =
    LoadSettings.builder().setLabel(label).setSchema(CoreSchemaInstance).build()

  /** How many levels a configuration may nest, each mapping and sequence one, the root's among them: snakeyaml-engine
    * composes a document, and this module walks it, by recursion with no limit of its own, and the default stack of 1
    * MB overflows before 1 000 levels of block mappings, a `StackOverflowError` that may leave a class broken for the
    * JVM (`format.md` §13 item 10). 256, as Psi's default.
    */
  private[yaml] def DepthLimit: Int = 256

  /** Composes a single YAML document, returning an empty mapping for an empty input. */
  private[yaml] def compose(settings: LoadSettings, reader: Reader): YamlNode =
    validated(
      new Composer(settings, new Bounded(new ParserImpl(settings, new StreamReader(settings, reader))))
        .getSingleNode
        .orElse(emptyMapping))

  private[yaml] def tooDeep(location: Location): YamlEngineException =
    new YamlEngineException(s"A configuration nests deeper than $DepthLimit levels at ${location.description}")

  /** Rejects a node that an alias makes contain itself, which a config tree cannot hold and `merge` would recurse into
    * forever, and a tree deeper than [[DepthLimit]] once its aliases are followed: an anchor nested deep and used deep
    * makes the tree deeper than its source, which the parser has bounded. An anchor reused anywhere outside its own
    * subtree is fine, and each node is visited once however often it is reused, its height kept for every later use.
    * Keys need no check: snakeyaml-engine refuses keys that are not scalars by default, recursive ones included, which
    * is also what makes `keyOf`'s cast safe.
    */
  private[yaml] def validated(root: YamlNode): YamlNode = {
    val heights = new IdentityHashMap[YamlNode, Integer]
    val inProgress = Collections.newSetFromMap(new IdentityHashMap[YamlNode, java.lang.Boolean]).asScala
    // the levels `node` opens, `above` levels standing above it in `parent`; never deeper than the limit, a node used
    // again past it reported in the parent, where the alias stands, rather than where its anchor does
    def visit(node: YamlNode, above: Int, parent: YamlNode): Int =
      Option(heights.get(node)).fold {
        if (!inProgress.add(node))
          throw new YamlEngineException(
            s"An alias makes the ${node.getNodeType.toString} at ${LocationImpl(node).description} contain itself")
        // a mapping or a sequence opens a level, past the limit failing before its children are visited
        def nested(children: Iterator[YamlNode]): Int =
          if (above + 1 > DepthLimit) throw tooDeep(LocationImpl(node))
          else 1 + children.map(visit(_, above + 1, node)).foldLeft(0)(math.max)
        val height = node match {
          case mapping: MappingNode => nested(mapping.getValue.asScala.iterator.map(_.getValueNode))
          case sequence: SequenceNode => nested(sequence.getValue.asScala.iterator)
          case _ => 0
        }
        inProgress -= node
        heights.put(node, height)
        height
      } { height =>
        if (above + height > DepthLimit) throw tooDeep(LocationImpl(parent))
        height.intValue
      }
    val _ = visit(root, 0, root)
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
  private[yaml] def entriesOf(mapping: MappingNode): Vector[(String, NodeTuple)] =
    mapping.getValue.asScala.reverseIterator.map(tuple => keyOf(tuple) -> tuple).distinctBy(_._1).toVector.reverse

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
  private[yaml] def merge(previous: YamlNode, next: YamlNode): YamlNode = new Merging().merge(previous, next)

  /** Overlays the mapping `overlay` onto `base`, key by key. */
  private[yaml] def mergeMappings(base: MappingNode, overlay: MappingNode): MappingNode =
    new Merging().mappings(base, overlay)

  /** `node` as YAML, every tag written in the source written again ([[Written]]). */
  private[yaml] def render(node: YamlNode): String = {
    val events = new Serialize(DumpOptions).serializeOne(node).asScala.toVector
    new Present(DumpOptions).emitToString(Written(node, events).iterator.asJava)
  }
}
