package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.events.{AliasEvent, CollectionStartEvent, Event, ScalarEvent}
import org.snakeyaml.engine.v2.exceptions.YamlEngineException
import org.snakeyaml.engine.v2.parser.Parser

import scala.collection.mutable
import scala.jdk.OptionConverters.*

/** `parser`, failing before the composer, which recurses, follows it where it cannot go: at the start of a mapping or a
  * sequence past [[DepthLimit]], and at an alias to a mapping or a sequence not yet ended, which would make it contain
  * itself — a cycle snakeyaml-engine would follow for ever as a merge key, `a: &a {<<: *a}`, and a config tree cannot
  * hold anyway. The parser and the scanner keep stacks of their own.
  */
private[yaml] final class Bounded(parser: Parser) extends Parser {
  private var depth = 0

  /** By anchor, what it names now, when that is a mapping or a sequence not yet ended: its kind and location. */
  private val open = mutable.Map.empty[String, String]

  /** The anchors of the mappings and sequences not yet ended, innermost first; none for one with no anchor. */
  private var anchors: List[Option[String]] = Nil

  override def checkEvent(id: Event.ID): Boolean = parser.checkEvent(id)

  override def peekEvent(): Event = parser.peekEvent()

  override def hasNext: Boolean = parser.hasNext

  override def next(): Event = {
    val event = parser.next()
    event match {
      case start: CollectionStartEvent =>
        depth += 1
        if (depth > DepthLimit) throw tooDeep(LocationImpl(event.getStartMark.toScala))
        val anchor = start.getAnchor.toScala.map(_.getValue)
        val kind = if (event.getEventId == Event.ID.MappingStart) "MAPPING" else "SEQUENCE"
        anchor.foreach(open(_) = kind + " at " + LocationImpl(event.getStartMark.toScala).description)
        anchors = anchor :: anchors
      case _ if event.getEventId == Event.ID.MappingEnd || event.getEventId == Event.ID.SequenceEnd =>
        depth -= 1
        anchors.head.foreach(open -= _)
        anchors = anchors.tail
      // an anchor defined again names the scalar now
      case scalar: ScalarEvent => scalar.getAnchor.toScala.foreach(anchor => open -= anchor.getValue)
      case alias: AliasEvent =>
        open.get(alias.getAlias.getValue).foreach(node =>
          throw new YamlEngineException(s"An alias makes the $node contain itself"))
      case _ =>
    }
    event
  }
}
