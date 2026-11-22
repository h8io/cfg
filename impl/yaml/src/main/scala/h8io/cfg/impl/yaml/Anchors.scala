package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.common.Anchor
import org.snakeyaml.engine.v2.nodes.Node as YamlNode
import org.snakeyaml.engine.v2.serializer.AnchorGenerator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.OptionConverters.*

/** Names the anchors of one rendering: a node keeps the name written in its source while no node written before it has
  * taken it, and takes a fresh one, `id` and a number, that no node has, otherwise. Overlaid files may each name an
  * anchor alike, and an alias written after the second would refer to it.
  */
private[yaml] final class Anchors extends AnchorGenerator {
  private val taken = mutable.Set.empty[String]
  private var last = 0

  def nextAnchor(node: YamlNode): Anchor = {
    val name = node.getAnchor.toScala.map(_.getValue).filterNot(taken).getOrElse(fresh())
    taken += name
    new Anchor(name)
  }

  @tailrec
  private def fresh(): String = {
    last += 1
    val name = "id" + String.format(java.util.Locale.ROOT, "%03d", Int.box(last))
    if (taken(name)) fresh() else name
  }
}
