package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.api.LoadSettings
import org.snakeyaml.engine.v2.comments.CommentLine
import org.snakeyaml.engine.v2.common.Anchor
import org.snakeyaml.engine.v2.composer.Composer
import org.snakeyaml.engine.v2.events.{CollectionStartEvent, ScalarEvent}
import org.snakeyaml.engine.v2.nodes.{Node as YamlNode, SequenceNode}
import org.snakeyaml.engine.v2.parser.Parser

import java.util.{List as JList, Optional}
import scala.jdk.OptionConverters.*

/** A [[Composer]] that remembers which tags were actually written in the source.
  *
  * The composer resolves a tag for every node, so by the time a node exists there is no way left to tell `42` from
  * `!!int 42`. This subclass peeks at the event before the node is built and records the tag from the event — absent
  * for an inferred tag — as a node property, which [[tagOf]] reads back.
  */
private[yaml] final class TaggedComposer(settings: LoadSettings, source: Parser) extends Composer(settings, source) {
  override protected def composeScalarNode(anchor: Optional[Anchor], blockComments: JList[CommentLine]): YamlNode = {
    val tag = parser.peekEvent.asInstanceOf[ScalarEvent].getTag
    tagged(super.composeScalarNode(anchor, blockComments), tag)
  }

  override protected def composeSequenceNode(anchor: Optional[Anchor]): SequenceNode = {
    val tag = parser.peekEvent.asInstanceOf[CollectionStartEvent].getTag
    tagged(super.composeSequenceNode(anchor), tag)
  }

  override protected def composeMappingNode(anchor: Optional[Anchor]): YamlNode = {
    val tag = parser.peekEvent.asInstanceOf[CollectionStartEvent].getTag
    tagged(super.composeMappingNode(anchor), tag)
  }

  private def tagged[N <: YamlNode](node: N, tag: Optional[String]): N = {
    tag.toScala.filter(_ != NonSpecificTag).foreach(value => node.setProperty(TagProperty, value))
    node
  }
}
