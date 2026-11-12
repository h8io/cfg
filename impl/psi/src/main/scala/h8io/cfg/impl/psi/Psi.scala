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
  * Deep merge, the last one wins, in one rule for duplicate keys, dotted keys and the sources given: maps merge with
  * their tags united, anything else is replaced; `:=` replaces a map too. Keys keep the order they first appeared in.
  * No sources give an empty root map (§7).
  *
  * ==Launch overrides==
  * A value the sources define that is not a container, at a path of keys, is replaced by the system property of that
  * path, `db.url`, if there is one (§9). The environment is not read.
  *
  * ==Substitutions==
  * `\${path}` grafts the node at `path` whole, looked up in the overridden tree; `\${?path}` with nothing there drops
  * its field or element, and reads as empty in a concatenation (§9).
  *
  * ==Directives==
  * `#name(…)` is expanded by the handler registered for it, while its source is read (§6); [[builder]] builds a loader
  * with handlers of its own. `#include` is reported as [[NotSupportedYet]] until the plan's step 11 lands.
  *
  * ==Markers==
  * A triple-quoted block's built-in markers shape its lines; a loader's own markers, registered through the [[builder]],
  * are functions of the block's finished value, run left to right after its substitutions are resolved (§5).
  */
object Psi {

  /** A builder of a loader with directives of its own. */
  def builder: PsiBuilder = PsiBuilder.empty

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given (§10). */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] = loader.apply(urls*)

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] = loader.parse(name, text)

  private def loader: PsiLoader = new PsiLoader(Map.empty, Set.empty, Map.empty, None)

  /** Loads `sources` with no directive of its own, their values overridden by `properties`. */
  private[psi] def load(
      sources: Seq[Either[PsiError, Source]],
      properties: Map[String, String]): Either[PsiErrors, Node.IMap[Id.Root]] =
    new PsiLoader(Map.empty, Set.empty, Map.empty, Some(properties)).load(sources)
}
