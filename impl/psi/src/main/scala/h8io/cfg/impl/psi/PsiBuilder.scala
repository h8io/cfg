package h8io.cfg.impl.psi

import java.time.Duration
import scala.collection.mutable

/** Builds a [[PsiLoader]] (`format.md` §5, §6): every method gives a new builder. */
final class PsiBuilder private (
    directives: Vector[Directive],
    disabled: Vector[String],
    markers: Vector[Marker],
    properties: Option[Map[String, String]],
    depthLimit: Int,
    nodeLimit: Int,
    characterLimit: Int,
    readBytes: Int,
    readTime: Duration) {

  /** Adds `directives` to those registered, the built-in `#include` among them. */
  def withDirectives(directives: Directive*): PsiBuilder =
    new PsiBuilder(this.directives ++ directives, disabled, markers, properties, depthLimit, nodeLimit, characterLimit,
      readBytes, readTime)

  /** Turns the directive `name` off for good, whether a handler for it is registered or not: a call of it is a
    * [[DisabledDirective]].
    */
  def disableDirective(name: String): PsiBuilder =
    new PsiBuilder(
      directives, disabled :+ name, markers, properties, depthLimit, nodeLimit, characterLimit, readBytes, readTime)

  /** Adds `markers` to those registered, the built-in `<`, `>`, `width` and `+` among them. */
  def withMarkers(markers: Marker*): PsiBuilder =
    new PsiBuilder(directives, disabled, this.markers ++ markers, properties, depthLimit, nodeLimit, characterLimit,
      readBytes, readTime)

  /** Overrides the sources' values from `properties` rather than from `sys.props`, read at every load; an empty map
    * turns the overrides off (§9).
    */
  def withSystemProperties(properties: Map[String, String]): PsiBuilder =
    new PsiBuilder(
      directives, disabled, markers, Some(properties), depthLimit, nodeLimit, characterLimit, readBytes, readTime)

  /** Limits the depth of the tree loaded, and of the resolution of its substitutions, to `limit`, at least 1, rather
    * than to 256 (`format.md` §11). The default holds on a thread with the JVM's default stack; lower it for a thread
    * with a smaller one.
    */
  def withDepthLimit(limit: Int): PsiBuilder =
    new PsiBuilder(directives, disabled, markers, properties, limit, nodeLimit, characterLimit, readBytes, readTime)

  /** Limits what resolving the substitutions adds to the tree loaded, rather than to 1 000 000 nodes and 10 000 000
    * characters (`format.md` §11): `nodes`, the nodes copied by grafts, and `characters`, those of the scalars built by
    * concatenations, each at least 0. The sources' own nodes and text are not counted.
    */
  def withSizeLimit(nodes: Int, characters: Int): PsiBuilder =
    new PsiBuilder(directives, disabled, markers, properties, depthLimit, nodes, characters, readBytes, readTime)

  /** Limits reading the sources of a load, rather than to 10 000 000 bytes and 60 seconds (`format.md` §6, §11):
    * `bytes` in all, every read counted, a source read twice twice, each at least 0; and `time` from the start of the
    * load, not negative, which a wait already begun may outrun by the read timeout at most. A source given as text is
    * not counted.
    */
  def withReadLimit(bytes: Int, time: Duration): PsiBuilder =
    new PsiBuilder(directives, disabled, markers, properties, depthLimit, nodeLimit, characterLimit, bytes, time)

  /** The loader, or every error of its configuration: the directives' first, then the markers', then the limits'. */
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
    if (nodeLimit < 0 || characterLimit < 0) report(InvalidSizeLimit(nodeLimit, characterLimit))
    if (readBytes < 0 || readTime.isNegative) report(InvalidReadLimit(readBytes, readTime))
    errors.result().toList match {
      case Nil =>
        Right(
          new PsiLoader(
            directives.map(d => d.name -> d).toMap,
            disabled.toSet,
            markers.map(m => m.name -> m).toMap,
            properties,
            depthLimit,
            nodeLimit,
            characterLimit,
            readBytes,
            readTime))
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
    new PsiBuilder(
      Vector.empty,
      Vector.empty,
      Vector.empty,
      None,
      PsiLoader.DefaultDepthLimit,
      PsiLoader.DefaultNodeLimit,
      PsiLoader.DefaultCharacterLimit,
      ReadBudget.DefaultBytes,
      ReadBudget.DefaultTime
    )
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

/** A size limit below 0, of nodes or of characters. */
final case class InvalidSizeLimit(nodes: Int, characters: Int) extends PsiBuildError

/** A read limit below 0 bytes, or of a negative time. */
final case class InvalidReadLimit(bytes: Int, time: Duration) extends PsiBuildError

/** Every error of a [[PsiBuilder]]'s configuration, in the order the handlers were registered. */
final case class PsiBuildErrors(errors: ::[PsiBuildError])
