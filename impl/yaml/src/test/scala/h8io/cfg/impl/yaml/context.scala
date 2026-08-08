package h8io.cfg.impl.yaml

import org.snakeyaml.engine.v2.nodes.{MappingNode, Node as YamlNode, SequenceNode}

import java.io.StringReader
import java.net.URL

object context {
  val Label: String = "test"

  def resource(path: String): URL = getClass.getClassLoader.getResource(path)

  /** The value node stored under `key`, as the composer left it. */
  def valueOf(mapping: MappingNode, key: String): YamlNode = entriesOf(mapping).toMap.apply(key).getValueNode

  def parse(text: String): YamlNode = compose(settings(Label), new StringReader(text))

  implicit final class CfgContext(private val sc: StringContext) extends AnyVal {

    /** Composes the interpolated document, which is expected to be a mapping. */
    def yaml(args: Any*): MappingNode = parse(sc.s(args*)).asInstanceOf[MappingNode]

    /** Composes the interpolated document, which is expected to be a sequence. */
    def list(args: Any*): SequenceNode = parse(sc.s(args*)).asInstanceOf[SequenceNode]
  }
}
