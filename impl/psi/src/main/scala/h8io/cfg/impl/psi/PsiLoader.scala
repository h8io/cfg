package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.net.URL

/** A Psi loader with the directives, markers and formatters it was built with; [[Psi]] is the one with nothing added.
  */
final class PsiLoader private[psi] (settings: Settings) {
  import settings.{characterLimit, depthLimit, nodeLimit, readBytes, readTime}

  private val directives: Map[String, Directive] = settings.directives.toMap

  private val disabled: Set[String] = settings.disabled.toSet

  private val markers: Map[String, Marker] = settings.markers.toMap

  private val formatters: Map[String, Formatter] = settings.formatters.toMap

  /** The directives with the built-in ones among them, `#prop` reading the system properties as they are now. */
  private def handlers: Map[String, Directive] = {
    val prop = new Launch.Prop(settings.properties.getOrElse(sys.props.toMap))
    val env = new Launch.Env(settings.environment.getOrElse(sys.env))
    directives ++ Seq[(String, Directive)](Include.name -> Include, prop.name -> prop, env.name -> env)
  }

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given. */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val budget = new ReadBudget(readBytes, readTime)
    load(
      urls.flatMap(url =>
        Source.read(url.toString, url, budget) match {
          case Right(source) => source.map(Right(_)).toList
          case Left(error) => Left(error) :: Nil
        }),
      budget
    )
  }

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] =
    load(Right(Source(name, text)) :: Nil, new ReadBudget(readBytes, readTime))

  private[psi] def load(
      sources: Seq[Either[PsiError, Source]],
      budget: ReadBudget): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val handlers = this.handlers
    val nodes = new Expand.Nodes(nodeLimit)
    val parsed = Result.sequence(sources.map {
      case Right(source) =>
        Expand(source, handlers, disabled, markers, formatters, depthLimit, budget, nodes).map {
          case (document, included) =>
            Some((document, source.name, included))
        }
      case Left(error) => Result.failed(None, error)
    })
    for {
      // the read that went past the budget, its source and every read after it dropped
      documents <- parsed.copy(errors = PsiLoader.distinct(parsed.errors) ++ budget.error ++ nodes.error).toEither
      root <- (documents.flatten.toList match {
        case (document, name, included) :: rest =>
          val located = rest.map { case (document, name, _) => (document, SourceLocation(name, 1, 1)) }
          val merged = Merge(::((document, SourceLocation(name, 1, 1)), located)).flatMap(tree =>
            Resolve(tree, depthLimit, nodeLimit, characterLimit, nodes.used))
          // merging and resolving go by key, not by source: the errors are put back in the order of the sources,
          // those directives parsed right after the one that parsed them
          val sources = (name :: included) ++ rest.flatMap { case (_, name, included) => name :: included }
          Result(MapImpl(Id.Root, merged.value), PsiLoader.distinct(merged.errors).sortBy(_.location.order(sources)))
        case Nil => Result.pure(MapImpl(Id.Root, Tree.Map.empty(None, NoSourceLocation)))
      }).toEither
    } yield root
  }
}

private[psi] object PsiLoader {

  /** How deep a loaded tree, and the resolution of its substitutions, may go unless the builder says otherwise (§11).
    */
  def DefaultDepthLimit: Int = 256

  /** The names of the built-in directives, which no handler may take (§6). */
  val BuiltInDirectives: Seq[String] = Seq("include", "prop", "env")

  /** The names of the built-in formatters, which no handler may take (§9). */
  val BuiltInFormatters: Seq[String] = Seq("join")

  /** How many nodes resolving the substitutions may copy into the tree by grafts unless the builder says otherwise
    * (§11).
    */
  def DefaultNodeLimit: Int = 1000000

  /** How many characters the scalars built by concatenations may hold together unless the builder says otherwise (§11).
    */
  def DefaultCharacterLimit: Int = 10000000

  /** `errors` with each reported once: a fragment used more than once, or one source parsed by two, reports the same
    * errors.
    */
  def distinct(errors: List[PsiError]): List[PsiError] = errors.distinctBy(key)

  /** What tells `error` from another: the error itself, but for the exception one carries, which equals none but itself
    * while each parse of a source that cannot be read throws one of its own; it is told apart by its class and message.
    * Not the message of the error: that shortens what it quotes, and two errors told apart only past the cut would be
    * one.
    */
  private def key(error: PsiError): Any =
    error match {
      case SourceUnreadable(source, cause) => (Unreadable, source, cause.toString)
      case DirectiveFailed(at, name, IncludeUnreadable(target, cause)) => (Unreadable, at, name, target, cause.toString)
      case _ => error
    }

  /** Marks a key built from an error that cannot be read, so that it equals no error. */
  private case object Unreadable
}
