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
      documents <- parsed.toEither
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
}

/** Builds a [[PsiLoader]] (`format.md` §5, §6): every method gives a new builder. */
final class PsiBuilder private (
    directives: Vector[Directive],
    disabled: Vector[String],
    markers: Vector[Marker],
    properties: Option[Map[String, String]],
    depthLimit: Int) {

  /** Adds `directives` to those registered, the built-in `#include` among them. */
  def withDirectives(directives: Directive*): PsiBuilder =
    new PsiBuilder(this.directives ++ directives, disabled, markers, properties, depthLimit)

  /** Turns the directive `name` off for good, whether a handler for it is registered or not: a call of it is a
    * [[DisabledDirective]].
    */
  def disableDirective(name: String): PsiBuilder =
    new PsiBuilder(directives, disabled :+ name, markers, properties, depthLimit)

  /** Adds `markers` to those registered, the built-in `<`, `>`, `width` and `+` among them. */
  def withMarkers(markers: Marker*): PsiBuilder =
    new PsiBuilder(directives, disabled, this.markers ++ markers, properties, depthLimit)

  /** Overrides the sources' values from `properties` rather than from `sys.props`, read at every load; an empty map
    * turns the overrides off (§9).
    */
  def withSystemProperties(properties: Map[String, String]): PsiBuilder =
    new PsiBuilder(directives, disabled, markers, Some(properties), depthLimit)

  /** Limits the depth of the tree loaded, and of the resolution of its substitutions, to `limit`, at least 1, rather
    * than to 256 (`format.md` §11). The default holds on a thread with the JVM's default stack; lower it for a thread
    * with a smaller one.
    */
  def withDepthLimit(limit: Int): PsiBuilder = new PsiBuilder(directives, disabled, markers, properties, limit)

  /** The loader, or every error of its configuration: the directives' first, then the markers', then the depth limit's.
    */
  def build: Either[PsiBuildErrors, PsiLoader] = {
    val errors = Vector.newBuilder[PsiBuildError]
    def report(error: PsiBuildError): Unit = {
      errors += error
      ()
    }
    def parameters(parameters: Seq[Parameter], duplicate: String => PsiBuildError): Unit = {
      val names = mutable.Set.empty[String]
      parameters.foreach(parameter => if (!names.add(parameter.name)) report(duplicate(parameter.name)))
    }
    val directiveNames = mutable.Set(PsiBuilder.Include)
    directives.foreach { directive =>
      if (!directiveNames.add(directive.name)) report(DuplicateDirective(directive.name))
      if (disabled.contains(directive.name)) report(DisabledDirectiveRegistered(directive.name))
      parameters(directive.parameters, DuplicateParameter(directive.name, _))
    }
    val markerNames = mutable.Set(PsiBuilder.BuiltInMarkers*)
    markers.foreach { marker =>
      if (!PsiBuilder.isMarkerName(marker.name)) report(InvalidMarkerName(marker.name))
      if (!markerNames.add(marker.name)) report(DuplicateMarker(marker.name))
      parameters(marker.parameters, DuplicateMarkerParameter(marker.name, _))
    }
    if (depthLimit < 1) report(InvalidDepthLimit(depthLimit))
    errors.result().toList match {
      case Nil =>
        Right(
          new PsiLoader(
            directives.map(d => d.name -> d).toMap,
            disabled.toSet,
            markers.map(m => m.name -> m).toMap,
            properties,
            depthLimit))
      case first :: rest => Left(PsiBuildErrors(::(first, rest)))
    }
  }
}

private[psi] object PsiBuilder {
  private def Include: String = "include"

  private def BuiltInMarkers: Seq[String] = Seq("<", ">", "width", "+")

  /** A run of the marker symbols, or a name by the key rule, as the syntax reads a marker's name (§5). */
  private def isMarkerName(name: String): Boolean =
    name.nonEmpty &&
      (name.forall(MarkerParser.isSymbol) ||
        Lexer.isNameStart(name.codePointAt(0)) && name.codePoints.allMatch(cp => Lexer.isNamePart(cp)))

  private[psi] def empty: PsiBuilder =
    new PsiBuilder(Vector.empty, Vector.empty, Vector.empty, None, PsiLoader.DefaultDepthLimit)
}

/** An error in the configuration of a [[PsiBuilder]]. */
sealed trait PsiBuildError

/** A directive registered under a name already taken, `include` included. */
final case class DuplicateDirective(name: String) extends PsiBuildError

/** A directive registered under a name that is disabled. */
final case class DisabledDirectiveRegistered(name: String) extends PsiBuildError

/** A directive declaring a parameter twice. */
final case class DuplicateParameter(directive: String, parameter: String) extends PsiBuildError

/** A marker registered under a name no marker can be written by: neither a run of symbols nor a name by the key rule.
  */
final case class InvalidMarkerName(name: String) extends PsiBuildError

/** A marker registered under a name already taken, a built-in one's included. */
final case class DuplicateMarker(name: String) extends PsiBuildError

/** A marker declaring a parameter twice. */
final case class DuplicateMarkerParameter(marker: String, parameter: String) extends PsiBuildError

/** A depth limit below 1. */
final case class InvalidDepthLimit(limit: Int) extends PsiBuildError

/** Every error of a [[PsiBuilder]]'s configuration, in the order the handlers were registered. */
final case class PsiBuildErrors(errors: ::[PsiBuildError])
