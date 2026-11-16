package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.events.*
import org.snakeyaml.engine.v2.nodes.{MappingNode, Node as YamlNode, SequenceNode}

import scala.jdk.CollectionConverters.*

/** The events snakeyaml-engine's serializer gives for `root`, with the tag of every node whose tag was written in the
  * source made explicit. The serializer leaves a tag out wherever the resolver would infer it, `!!str` on `hello` and
  * `!!map` on any mapping, never asking whether it was written; [[tagOf]] reports it, so the text would read back
  * without it.
  *
  * The events follow the nodes in the order the serializer visits them: a node it has already written is an alias, with
  * nothing below it.
  */
private[yaml] final class Written private (events: Vector[Event]) {
  private val out = Vector.newBuilder[Event]
  private var next = 0

  private def take(): Event = {
    val event = events(next)
    next += 1
    event
  }

  private def visit(node: YamlNode): Unit =
    take() match {
      case alias: AliasEvent => out += alias
      case scalar: ScalarEvent =>
        out +=
          (if (node.isResolved) scalar
           else
             new ScalarEvent(
               scalar.getAnchor,
               scalar.getTag,
               new ImplicitTuple(false, false),
               scalar.getValue,
               scalar.getScalarStyle,
               scalar.getStartMark,
               scalar.getEndMark))
      case start: SequenceStartEvent =>
        out +=
          (if (node.isResolved) start
           else
             new SequenceStartEvent(
               start.getAnchor,
               start.getTag,
               false,
               start.getFlowStyle,
               start.getStartMark,
               start.getEndMark))
        node.asInstanceOf[SequenceNode].getValue.asScala.foreach(visit)
        out += take()
      case start: MappingStartEvent =>
        out +=
          (if (node.isResolved) start
           else
             new MappingStartEvent(
               start.getAnchor,
               start.getTag,
               false,
               start.getFlowStyle,
               start.getStartMark,
               start.getEndMark))
        node.asInstanceOf[MappingNode].getValue.asScala.foreach { tuple =>
          visit(tuple.getKeyNode)
          visit(tuple.getValueNode)
        }
        out += take()
      // the serializer writes a node with one of the above, and the composer reads no comments
      case other => throw new IllegalStateException("An event of no node, " + other.toString + ", for a node")
    }

  private def result(root: YamlNode): Vector[Event] = {
    val first = events.indexWhere(_.isInstanceOf[NodeEvent])
    out ++= events.take(first)
    next = first
    visit(root)
    out ++= events.drop(next)
    out.result()
  }
}

private[yaml] object Written {
  def apply(root: YamlNode, events: Vector[Event]): Vector[Event] = new Written(events).result(root)
}
