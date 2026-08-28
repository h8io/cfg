package h8io.cfg.impl.yaml

import h8io.cfg.Location
import org.snakeyaml.engine.v2.exceptions.Mark
import org.snakeyaml.engine.v2.nodes.Node as YamlNode

import scala.jdk.OptionConverters.*

private[yaml] final case class LocationImpl(mark: Option[Mark]) extends Location {
  override def description: String =
    mark.fold(LocationImpl.Unknown)(at => s"${at.getName}: ${at.getLine + 1}:${at.getColumn + 1}")

  /** `Mark.toString` renders a multi-line snippet of the source; nodes print better with just the position. */
  override def toString: String = description
}

private[yaml] object LocationImpl {

  /** Rendered for nodes that carry no mark, i.e. the synthetic root of an empty configuration. */
  private[yaml] def Unknown: String = "<unknown>"

  def apply(node: YamlNode): Location = LocationImpl(node.getStartMark.toScala)
}
