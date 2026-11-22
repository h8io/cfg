package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.net.URL

/** Ψ (Psi) backend for the `cfg` protocol (`design/format.md`).
  *
  * Load one or more Psi sources and receive a `Node.IMap[Id.Root]`, or every error found on the way.
  *
  * ==Phases==
  * Each source is read and parsed on its own, so an error in one never hides an error in another, and the parser goes
  * on past a broken field to report the next (§11). Directives are expanded, the sources merged in order, the
  * substitutions resolved, and the result wrapped (§8). Errors come in two batches: the syntax and directive errors of
  * every source first; if there were none, the merge and substitution errors.
  *
  * ==Merging==
  * Deep merge, the last one wins, in one rule for duplicate keys, dotted keys and the sources given: maps merge,
  * keeping the later tag if one is written there and the earlier otherwise, anything else is replaced; `:=` replaces a
  * map too. Keys keep the order they first appeared in. No sources give an empty root map (§7).
  *
  * ==Launch values==
  * Nothing set at launch reaches a source on its own: `#prop(name)` reads a system property, or a map of those under
  * it, and `#env(name)` an environment variable, where the call stands (§6).
  *
  * ==Substitutions==
  * `\${path}` grafts the node at `path` whole, looked up in the merged tree; `\${?path}` with nothing there drops its
  * field or element, and reads as empty in a concatenation. `\${path; name(…)}` turns the node into a string with the
  * formatter registered as `name`, the built-in `join` or one of the loader's own (§9).
  *
  * ==Directives==
  * `#name(…)` is expanded by the handler registered for it, while its source is read (§6); [[builder]] builds a loader
  * with handlers of its own. The built-in `#include(file: …)`, `#include(resource: …)` and `#include(url: …)` read
  * another Psi source where the call stands, and `#prop` and `#env` a value set at launch. `cfg-psi-yaml` and
  * `cfg-psi-hocon` give handlers that import a YAML or a HOCON file.
  *
  * ==Markers==
  * A triple-quoted block's built-in markers shape its lines; a loader's own markers, registered through the
  * [[builder]], are functions of the block's finished value, run left to right after its substitutions are resolved
  * (§5).
  */
object Psi {

  /** A builder of a loader with directives, markers, formatters or limits of its own. */
  def builder: PsiBuilder = PsiBuilder.empty

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given (§10). */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] = loader.apply(urls*)

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] = loader.parse(name, text)

  private val loader: PsiLoader = new PsiLoader(Settings())

  /** Loads `sources` with no directive of its own, `#prop` reading `properties`. */
  private[psi] def load(
      sources: Seq[Either[PsiError, Source]],
      properties: Map[String, String]): Either[PsiErrors, Node.IMap[Id.Root]] =
    new PsiLoader(Settings(properties = Some(properties)))
      .load(sources, new ReadBudget(ReadBudget.DefaultBytes, ReadBudget.DefaultTime))
}
