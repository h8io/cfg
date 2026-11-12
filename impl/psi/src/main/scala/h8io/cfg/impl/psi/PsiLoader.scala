package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.net.URL

/** A Psi loader with the directives it was built with (`format.md` §6); [[Psi]] is the one with nothing added. */
final class PsiLoader private[psi] (
    directives: Map[String, Directive],
    disabled: Set[String],
    properties: Option[Map[String, String]]) {

  /** Loads and merges the sources at `urls`, in order, each named in locations by its URL as given (§10). */
  def apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]] = load(urls.map(url => Source.read(url.toString, url)))

  /** Loads a source held as `text`, named `name` in locations. */
  def parse(name: String, text: String): Either[PsiErrors, Node.IMap[Id.Root]] = load(Right(Source(name, text)) :: Nil)

  private[psi] def load(sources: Seq[Either[PsiError, Source]]): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val parsed = Result.sequence(sources.map {
      case Right(source) => Expand(source, directives, disabled).map(document => Some((document, source.name)))
      case Left(error) => Result.failed(None, error)
    })
    for {
      documents <- parsed.toEither
      root <- (documents.flatten.toList match {
        case (document, name) :: rest =>
          val located = rest.map { case (document, name) => (document, SourceLocation(name, 1, 1)) }
          val overrides = properties.getOrElse(sys.props.toMap)
          val merged = Merge(::((document, SourceLocation(name, 1, 1)), located)).flatMap(tree =>
            Resolve(Override(tree, overrides)))
          // merging and resolving go by key, not by source: the errors are put back in the order of the sources
          val sources = name :: rest.map(_._2)
          Result(MapImpl(Id.Root, merged.value), merged.errors.sortBy(_.location.order(sources)))
        case Nil => Result.pure(MapImpl(Id.Root, Tree.Map.empty(Set.empty, NoSourceLocation)))
      }).toEither
    } yield root
  }
}

/** Builds a [[PsiLoader]] (`format.md` §6): every method gives a new builder. */
final class PsiBuilder private (
    directives: Vector[Directive],
    disabled: Vector[String],
    properties: Option[Map[String, String]]) {

  /** Adds `directives` to those registered, the built-in `#include` among them. */
  def withDirectives(directives: Directive*): PsiBuilder =
    new PsiBuilder(this.directives ++ directives, disabled, properties)

  /** Turns the directive `name` off for good, whether a handler for it is registered or not: a call of it is a
    * [[DisabledDirective]].
    */
  def disableDirective(name: String): PsiBuilder = new PsiBuilder(directives, disabled :+ name, properties)

  /** Overrides the sources' values from `properties` rather than from `sys.props`, read at every load; an empty map
    * turns the overrides off (§9).
    */
  def withSystemProperties(properties: Map[String, String]): PsiBuilder =
    new PsiBuilder(directives, disabled, Some(properties))

  /** The loader, or every error of its configuration. */
  def build: Either[PsiBuildErrors, PsiLoader] = {
    val errors = Vector.newBuilder[PsiBuildError]
    def report(error: PsiBuildError): Unit = {
      errors += error
      ()
    }
    val _ = directives.foldLeft(Set(PsiBuilder.Include)) { (seen, directive) =>
      if (seen(directive.name)) report(DuplicateDirective(directive.name))
      if (disabled.contains(directive.name)) report(DisabledDirectiveRegistered(directive.name))
      val _ = directive.parameters.map(_.name).foldLeft(Set.empty[String]) { (names, parameter) =>
        if (names(parameter)) report(DuplicateParameter(directive.name, parameter))
        names + parameter
      }
      seen + directive.name
    }
    errors.result().toList match {
      case Nil => Right(new PsiLoader(directives.map(d => d.name -> d).toMap, disabled.toSet, properties))
      case first :: rest => Left(PsiBuildErrors(::(first, rest)))
    }
  }
}

object PsiBuilder {
  private def Include: String = "include"

  private[psi] def empty: PsiBuilder = new PsiBuilder(Vector.empty, Vector.empty, None)
}

/** An error in the configuration of a [[PsiBuilder]]. */
sealed trait PsiBuildError

/** A directive registered under a name already taken, `include` included. */
final case class DuplicateDirective(name: String) extends PsiBuildError

/** A directive registered under a name that is disabled. */
final case class DisabledDirectiveRegistered(name: String) extends PsiBuildError

/** A directive declaring a parameter twice. */
final case class DuplicateParameter(directive: String, parameter: String) extends PsiBuildError

/** Every error of a [[PsiBuilder]]'s configuration, in the order the directives were registered. */
final case class PsiBuildErrors(errors: ::[PsiBuildError])
