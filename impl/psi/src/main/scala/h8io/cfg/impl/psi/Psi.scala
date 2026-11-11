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
  * ==Not yet==
  * Directives and substitutions are reported as [[NotSupportedYet]] until the plan's steps 9–11 land.
  */
object Psi {

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given (§10). */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] =
    load(urls.map(url => Source.read(url.toString, url)))

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] = load(Right(Source(name, text)) :: Nil)

  private def load(sources: Seq[Either[PsiError, Source]]): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val parsed = Result.sequence(sources.map {
      case Right(source) => Parser.parse(source).flatMap(expand).map(document => Some((document, source.name)))
      case Left(error) => Result.failed(None, error)
    })
    for {
      documents <- parsed.toEither
      root <- (documents.flatten.toList match {
        case (document, name) :: rest =>
          val located = rest.map { case (document, name) => (document, SourceLocation(name, 1, 1)) }
          Merge(::((document, SourceLocation(name, 1, 1)), located)).flatMap(resolve).map(map =>
            MapImpl(Id.Root, map, map.location))
        // the tree's site is never shown: the view has a location of its own
        case Nil => Result.pure(MapImpl(Id.Root, Tree.Map.empty(Set.empty, SourceLocation("", 1, 1)), NoSourceLocation))
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

  /** Resolves the substitutions of the merged tree (§9): none yet, each reported. */
  private def resolve(root: Tree.Map): Result[Tree.Map] = {
    val errors = List.newBuilder[PsiError]
    def unsupported(path: Id.Branch, optional: Boolean, location: SourceLocation): Unit = {
      errors += NotSupportedYet(location, (if (optional) "${?" else "${") + path.path + "}")
      ()
    }
    def walk(value: Tree.Value): Unit =
      value match {
        case map: Tree.Map => map.keys.foreach(key => walk(map.entries(key)))
        case sequence: Tree.Sequence => sequence.elements.foreach(walk)
        case Tree.Substitution(path, optional, _, location) => unsupported(path, optional, location)
        case Tree.Concatenation(parts, _, _) =>
          parts.foreach {
            case Token.Reference(path, optional, location) => unsupported(path, optional, location)
            case Token.Text(_) => ()
          }
        case _: Tree.Scalar | _: Tree.Null => ()
      }
    walk(root)
    Result(root, errors.result())
  }
}
