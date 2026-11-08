package h8io.cfg.impl.hocon

import com.typesafe.config.{ConfigFactory, ConfigParseOptions}
import h8io.cfg.{Id, Node}
import io.h8.config.yaml.{ConfigLoader, YamlConfigFactory}

import java.net.URL

/** HOCON/YAML backend for the `cfg` protocol.
  *
  * Load one or more HOCON or YAML files and receive a `Node.IMap[Id.Root]`.
  *
  * ==Merging and substitution==
  * When multiple URLs are supplied they are merged in order — later files take precedence over earlier ones
  * (last-wins), following Typesafe Config's `withFallback` semantics. After merging, `ConfigFactory.load` is called so
  * that variable substitutions are resolved before the tree is returned.
  *
  * ==YAML support==
  * YAML files are parsed via `io.h8:typesafe-config-yaml` with `stringsOnly = true`. This aligns with the protocol
  * contract: `Node.IScalar` always carries a raw string, and converting it is up to the consumer.
  *
  * HOCON and JSON files, however, go through Typesafe Config's own parser, which infers types (booleans, numbers,
  * lists, etc.) before this library can see the values. Scalars are then recovered by calling `unwrapped.toString` on
  * the parsed Java object, so the string stored in `Node.IScalar` may not match the original text in the file exactly
  * (e.g. a number written as `1e5` may come back as `100000.0`).
  *
  * YAML tags are not propagated; `tags` is always empty.
  *
  * ==Thread safety==
  * The shared `ConfigLoader` is stateless after construction and safe for concurrent use.
  */
object HOCON {
  private val yamlFactory = YamlConfigFactory.builder().stringsOnly(true).build()

  /** For the standard application config, where a missing `application.conf` is normal. */
  private val loader = ConfigLoader.builder().yamlFactory(yamlFactory).build()

  /** For URLs passed explicitly, where a missing file is a mistake. An `include` inside one stays optional unless it is
    * written `include required(…)`.
    */
  private val urlLoader =
    ConfigLoader.builder()
      .yamlFactory(yamlFactory)
      .parseOptions(ConfigParseOptions.defaults().setAllowMissing(false))
      .build()

  /** Loads and merges the given HOCON/YAML URLs into a single config tree.
    *
    * @param urls
    *   zero or more URLs to HOCON or YAML files; an empty argument list returns the result of `ConfigFactory.load`
    *   (system properties + reference.conf + application.conf on the classpath)
    * @return
    *   the root map node of the merged, substitution-resolved config tree
    * @note
    *   a `ConfigException.IO` is thrown if one of the URLs cannot be read, whatever its syntax
    */
  def apply(urls: URL*): Node.IMap[Id.Root] =
    wrap(
      Id.Root,
      urls.iterator
        .map(urlLoader.parseURL)
        .reduceOption((p, n) => n.withFallback(p))
        .map(ConfigFactory.load)
        .getOrElse(loader.load)
        .root()
    )
}
