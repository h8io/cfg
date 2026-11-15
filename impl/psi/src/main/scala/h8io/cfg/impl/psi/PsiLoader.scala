package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.net.URL
import scala.collection.mutable

/** A Psi loader with the directives and markers it was built with (`format.md` §5, §6); [[Psi]] is the one with nothing
  * added.
  */
final class PsiLoader private[psi] (
    directives: Map[String, Directive],
    disabled: Set[String],
    markers: Map[String, Marker],
    properties: Option[Map[String, String]],
    depthLimit: Int) {

  /** The directives with the built-in `#include` among them. */
  private val handlers: Map[String, Directive] = directives.updated(Include.name, Include)

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given (§10). */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] = load(urls.map(url => Source.read(url.toString, url)))

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] = load(Right(Source(name, text)) :: Nil)

  private[psi] def load(sources: Seq[Either[PsiError, Source]]): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val parsed = Result.sequence(sources.map {
      case Right(source) =>
        Expand(source, handlers, disabled, markers, depthLimit).map { case (document, included) =>
          Some((document, source.name, included))
        }
      case Left(error) => Result.failed(None, error)
    })
    for {
      documents <- parsed.copy(errors = PsiLoader.distinct(parsed.errors)).toEither
      root <- (documents.flatten.toList match {
        case (document, name, included) :: rest =>
          val located = rest.map { case (document, name, _) => (document, SourceLocation(name, 1, 1)) }
          val overrides = properties.getOrElse(sys.props.toMap)
          val merged = Merge(::((document, SourceLocation(name, 1, 1)), located)).flatMap(tree =>
            Resolve(Override(tree, overrides), depthLimit))
          // merging and resolving go by key, not by source: the errors are put back in the order of the sources,
          // those directives parsed right after the one that parsed them
          val sources = (name :: included) ++ rest.flatMap { case (_, name, included) => name :: included }
          Result(MapImpl(Id.Root, merged.value), merged.errors.sortBy(_.location.order(sources)))
        case Nil => Result.pure(MapImpl(Id.Root, Tree.Map.empty(Set.empty, NoSourceLocation)))
      }).toEither
    } yield root
  }
}

private[psi] object PsiLoader {

  /** How deep a loaded tree, and the resolution of its substitutions, may go unless the builder says otherwise (§11).
    */
  def DefaultDepthLimit: Int = 256

  /** `errors` with each reported once: a fragment used more than once, or one source parsed by two, reports the same
    * errors.
    */
  def distinct(errors: List[PsiError]): List[PsiError] = {
    val reported = mutable.HashSet.empty[Any]
    errors.filter(error => reported.add(key(error)))
  }

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
