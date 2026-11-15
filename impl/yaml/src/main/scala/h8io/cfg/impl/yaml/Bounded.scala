package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.events.Event
import org.snakeyaml.engine.v2.parser.Parser

import scala.jdk.OptionConverters.*

/** `parser`, failing at the start of a mapping or a sequence past [[DepthLimit]], before the composer, which recurses,
  * follows it there. The parser and the scanner keep stacks of their own.
  */
private[yaml] final class Bounded(parser: Parser) extends Parser {
  private var depth = 0

  override def checkEvent(id: Event.ID): Boolean = parser.checkEvent(id)

  override def peekEvent(): Event = parser.peekEvent()

  override def hasNext: Boolean = parser.hasNext

  override def next(): Event = {
    val event = parser.next()
    event.getEventId match {
      case Event.ID.MappingStart | Event.ID.SequenceStart =>
        depth += 1
        if (depth > DepthLimit) throw tooDeep(LocationImpl(event.getStartMark.toScala))
      case Event.ID.MappingEnd | Event.ID.SequenceEnd => depth -= 1
      case _ =>
    }
    event
  }
}
