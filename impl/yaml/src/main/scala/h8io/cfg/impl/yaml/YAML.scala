package h8io.cfg.impl.yaml

import h8io.cfg.{Id, Node}
import org.snakeyaml.engine.v2.api.YamlUnicodeReader
import org.snakeyaml.engine.v2.exceptions.YamlEngineException
import org.snakeyaml.engine.v2.nodes.{MappingNode, Node => YamlNode}

import java.net.URL

/** YAML backend for the `cfg` protocol.
  *
  * Load one or more YAML files and receive a `Node.IMap[Id.Root]` ready for decoding. Parsing is done by
  * `snakeyaml-engine` with the YAML 1.2 core schema, so `~`, `null`, `Null` and `NULL` are nulls, and `<<` merge keys
  * are resolved while composing.
  *
  * ==Merging==
  * When multiple URLs are supplied they are overlaid in order — later files take precedence over earlier ones
  * (last-wins). Mappings are merged recursively, key by key; scalars and sequences are replaced outright rather than
  * concatenated. An empty argument list yields an empty root map. Unlike the HOCON backend there is no substitution
  * step: a YAML file is taken exactly as written.
  *
  * ==Tags==
  * Unlike the HOCON backend, this one reports tags. `tag` holds the tag as it was written in the source, already
  * expanded through the tag handles in effect (`!!int` becomes `tag:yaml.org,2002:int`, a local `!postgres` stays
  * `!postgres`). It is `None` whenever the tag was inferred by the resolver rather than written down, and also for the
  * non-specific tag `!`, which says nothing about the type. Scalars are always carried as raw strings — a tag never
  * converts a value, it only records what the author asked for, and acting on it is the decoder's business.
  *
  * ==Duplicate keys==
  * A mapping that repeats a key keeps the last occurrence, matching the last-wins rule used for merging.
  *
  * ==Thread safety==
  * `apply` builds its own parser per call and shares no mutable state; the returned tree is immutable.
  */
object YAML {

  /** Loads and overlays the given YAML URLs into a single config tree.
    *
    * @param urls
    *   zero or more URLs to YAML files, each holding a single document; an empty argument list returns an empty root
    *   map
    * @return
    *   the root map node of the overlaid config tree
    * @note
    *   a `YamlEngineException` is thrown if a stream cannot be parsed, holds more than one document, or is rooted at
    *   something other than a mapping
    */
  def apply(urls: URL*): Node.IMap[Id.Root] =
    wrap(Id.Root, urls.iterator.map(read).reduceOption(merge).fold(emptyMapping)(rootOf))

  private def read(url: URL): YamlNode = {
    val stream = url.openStream()
    try compose(settings(url.toString), new YamlUnicodeReader(stream))
    finally stream.close()
  }

  private def rootOf(node: YamlNode): MappingNode =
    node match {
      case mapping: MappingNode => mapping
      case _ =>
        throw new YamlEngineException(
          s"A configuration root must be a mapping, ${node.getNodeType} found at ${LocationImpl(node).description}")
    }
}
