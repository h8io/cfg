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
  * ==Substitutions==
  * `\${path}` grafts the node at `path` whole, looked up in the merged tree, then in the system properties split at the
  * dots, then in the environment; `\${?path}` with nothing there drops its field or element, and reads as empty in a
  * concatenation (§9).
  *
  * ==Not yet==
  * Directives are reported as [[NotSupportedYet]] until the plan's steps 10–11 land.
  */
object Psi {

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given (§10). */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] =
    load(urls.map(url => Source.read(url.toString, url)), () => sys.props.toMap, () => sys.env)

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] =
    load(Right(Source(name, text)) :: Nil, () => sys.props.toMap, () => sys.env)

  /** Loads `sources`, looking up what the tree does not have in `properties` and `env`, read only if needed. */
  private[psi] def load(
      sources: Seq[Either[PsiError, Source]],
      properties: () => Map[String, String],
      env: () => Map[String, String]): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val parsed = Result.sequence(sources.map {
      case Right(source) => Parser.parse(source).flatMap(expand).map(document => Some((document, source.name)))
      case Left(error) => Result.failed(None, error)
    })
    for {
      documents <- parsed.toEither
      root <- (documents.flatten.toList match {
        case (document, name) :: rest =>
          val located = rest.map { case (document, name) => (document, SourceLocation(name, 1, 1)) }
          val merged = Merge(::((document, SourceLocation(name, 1, 1)), located)).flatMap(Resolve(_, properties, env))
          // merging and resolving go by key, not by source: the errors are put back in the order of the sources
          val sources = name :: rest.map(_._2)
          Result(MapImpl(Id.Root, merged.value), merged.errors.sortBy(_.location.order(sources)))
        case Nil => Result.pure(MapImpl(Id.Root, Tree.Map.empty(Set.empty, NoSourceLocation)))
      }).toEither
    } yield root
  }

  /** Expands the directives of a source (§6, §8): none yet, each reported, its field or element left out. */
  private def expand(document: Ast.Document): Result[Ast.Document] = {
    val errors = List.newBuilder[PsiError]
    def unsupported(call: Ast.Call): Unit = {
      errors += NotSupportedYet(call.location, "#" + call.name)
      ()
    }
    def statements(statements: Vector[Ast.Statement]): Vector[Ast.Statement] =
      statements.flatMap {
        case field: Ast.Field => expanded(field.value).map(value => field.copy(value = value))
        case Ast.DirectiveStatement(call, _) =>
          unsupported(call)
          None
      }
    def expanded(value: Ast.Value): Option[Ast.Value] =
      value match {
        case block: Ast.Block => Some(block.copy(statements = statements(block.statements)))
        case sequence: Ast.Sequence =>
          Some(
            sequence.copy(elements = sequence.elements.flatMap(e => expanded(e.value).map(v => e.copy(value = v)))))
        case Ast.DirectiveValue(call, _) =>
          unsupported(call)
          None
        case _ => Some(value)
      }
    val read = Ast.Document(statements(document.statements))
    Result(read, errors.result())
  }
}
