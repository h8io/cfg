package h8io.cfg.impl.yaml

import h8io.cfg.{Id, Node}
import org.snakeyaml.engine.v2.api.YamlUnicodeReader
import org.snakeyaml.engine.v2.exceptions.YamlEngineException
import org.snakeyaml.engine.v2.nodes.{MappingNode, Node as YamlNode}

import java.net.URL

/** YAML backend for the `cfg` protocol.
  *
  * Load one or more YAML files and receive a `Node.IMap[Id.Root]`. Parsing is done by `snakeyaml-engine` with the YAML
  * 1.2 core schema, so `~`, `null`, `Null` and `NULL` are nulls, and `<<` merge keys are resolved while composing.
  *
  * ==Merging==
  * When multiple URLs are supplied they are overlaid in order — later files take precedence over earlier ones
  * (last-wins). Mappings are merged recursively, key by key; scalars and sequences are replaced outright rather than
  * concatenated. An empty argument list yields an empty root map. Unlike the HOCON backend there is no substitution
  * step: a YAML file is taken exactly as written.
  *
  * ==Tags==
  * Unlike the HOCON backend, this one reports tags. `tag` holds the tag written in the source, expanded through the tag
  * handles in effect and without its leading `!`: `!!int` gives `tag:yaml.org,2002:int`, a local `!postgres` gives
  * `postgres`, and so does the verbatim `!<postgres>`, YAML allowing one tag per node. It is `None` whenever the tag
  * was inferred by the resolver rather than written down, and also for the non-specific tag `!`, which says nothing
  * about the type. Scalars are always carried as raw strings — a tag never converts a value, it only records what the
  * author asked for, and acting on it is up to the consumer.
  *
  * ==Depth==
  * A configuration nests at most 256 levels, each mapping and sequence one, the root's among them, aliases followed:
  * snakeyaml-engine composes a document by recursion with no limit of its own, and a deeper one would overflow the
  * stack. Past it, the load fails before the composer goes there.
  *
  * ==Reading==
  * A URL is read with a timeout of 10 seconds for the connection and 30 seconds for each wait for its next bytes, so
  * that a server that never answers fails the load rather than holding it forever. A `jar:` URL of a jar elsewhere than
  * in a file is fetched by the JVM, with no timeouts. How much a stream may hold is snakeyaml-engine's limit, 3 MB of
  * code points by default.
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
    *   a `YamlEngineException` is thrown if a stream cannot be parsed, holds more than one document, is rooted at
    *   something other than a mapping, has a key that is not a scalar, has a node that an alias makes contain itself,
    *   or nests deeper than 256 levels, aliases followed; every URL is checked on its own, before any overlaying. An
    *   `IOException` is thrown if a URL cannot be read, a `SocketTimeoutException` if it keeps the connection or its
    *   next bytes waiting past the timeouts.
    */
  def apply(urls: URL*): Node.IMap[Id.Root] = load(urls, ConnectTimeout, ReadTimeout)

  /** How long reading a URL waits for a connection, in milliseconds. */
  private[yaml] def ConnectTimeout: Int = 10000

  /** How long reading a URL waits for its next bytes, in milliseconds. */
  private[yaml] def ReadTimeout: Int = 30000

  /** `apply` with the timeouts given, in milliseconds. */
  private[yaml] def load(urls: Seq[URL], connectTimeout: Int, readTimeout: Int): Node.IMap[Id.Root] =
    wrap(
      Id.Root,
      urls.iterator
        .map(url => rootOf(read(url, connectTimeout, readTimeout)))
        .reduceOption(mergeMappings)
        .getOrElse(emptyMapping))

  /** `url` composed, read with the timeouts set: a URL connection has none of its own. */
  private def read(url: URL, connectTimeout: Int, readTimeout: Int): YamlNode = {
    val connection = url.openConnection()
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    val stream = connection.getInputStream
    try compose(settings(url.toString), new YamlUnicodeReader(stream))
    finally stream.close()
  }

  private def rootOf(node: YamlNode): MappingNode =
    node match {
      case mapping: MappingNode => mapping
      case _ =>
        throw new YamlEngineException(
          s"A configuration root must be a mapping, ${node.getNodeType.toString} found at ${LocationImpl(node).description}")
    }
}
