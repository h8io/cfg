package h8io.cfg.impl.psi

import java.time.Duration
import scala.collection.mutable

/** Builds a [[PsiLoader]]: every method gives a new builder. */
final class PsiBuilder private (settings: Settings) {

  /** Adds `directive`, called by `#name`, to those registered, the built-in `#include`, `#prop` and `#env` among them.
    */
  def withDirective(name: String, directive: Directive): PsiBuilder =
    new PsiBuilder(settings.copy(directives = settings.directives :+ (name -> directive)))

  /** Turns the directive `name` off for good, whether a handler for it is registered or not: a call of it is a
    * [[DisabledDirective]].
    */
  def disableDirective(name: String): PsiBuilder = new PsiBuilder(settings.copy(disabled = settings.disabled :+ name))

  /** Adds `marker`, written `name`, to those registered, the built-in `<`, `>`, `width` and `+` among them. */
  def withMarker(name: String, marker: Marker): PsiBuilder =
    new PsiBuilder(settings.copy(markers = settings.markers :+ (name -> marker)))

  /** Adds `formatter`, called by `name` in `\${path; name(…)}`, to those registered, the built-in `join` among them.
    */
  def withFormatter(name: String, formatter: Formatter): PsiBuilder =
    new PsiBuilder(settings.copy(formatters = settings.formatters :+ (name -> formatter)))

  /** The system properties `#prop` reads, rather than `sys.props` read at every load. */
  def withSystemProperties(properties: Map[String, String]): PsiBuilder =
    new PsiBuilder(settings.copy(properties = Some(properties)))

  /** The environment variables `#env` reads, rather than `sys.env`. */
  def withEnvironment(environment: Map[String, String]): PsiBuilder =
    new PsiBuilder(settings.copy(environment = Some(environment)))

  /** Limits the depth of the tree loaded, and of the resolution of its substitutions, to `limit`, at least 1, rather
    * than to 256. The default holds on a thread with the JVM's default stack; lower it for a thread with a smaller one.
    */
  def withDepthLimit(limit: Int): PsiBuilder = new PsiBuilder(settings.copy(depthLimit = limit))

  /** Limits what a load adds to the tree of its sources, rather than to 1 000 000 nodes and 10 000 000 characters :
    * `nodes`, those the fragments directives build bring, every use counted, and those grafts copy, and `characters`,
    * those of the scalars built by concatenations, each at least 0. The sources' own nodes and text are not counted.
    * Some 350 000 nodes fill a heap of 64 MB.
    */
  def withSizeLimit(nodes: Int, characters: Int): PsiBuilder =
    new PsiBuilder(settings.copy(nodeLimit = nodes, characterLimit = characters))

  /** Limits reading the sources of a load, rather than to 10 000 000 bytes and 60 seconds: `bytes` in all, every read
    * counted, a source read twice twice, each at least 0; and `time` from the start of the load, not negative, which a
    * wait already begun may outrun by the read timeout at most. A source given as text is not counted.
    */
  def withReadLimit(bytes: Int, time: Duration): PsiBuilder =
    new PsiBuilder(settings.copy(readBytes = bytes, readTime = time))

  /** The loader, or every error of its configuration: the directives' first, then the markers', the formatters', and
    * the limits'.
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
    import settings.*
    val directiveNames = mutable.Set(PsiLoader.BuiltInDirectives*)
    directives.foreach { case (name, directive) =>
      if (!directiveNames.add(name)) report(DuplicateDirective(name))
      if (disabled.contains(name)) report(DisabledDirectiveRegistered(name))
      parameters(directive.parameters, DuplicateParameter(name, _))
    }
    val markerNames = mutable.Set(PsiBuilder.BuiltInMarkers*)
    markers.foreach { case (name, marker) =>
      if (!PsiBuilder.isMarkerName(name)) report(InvalidMarkerName(name))
      if (!markerNames.add(name)) report(DuplicateMarker(name))
      parameters(marker.parameters, DuplicateMarkerParameter(name, _))
    }
    val formatterNames = mutable.Set(PsiLoader.BuiltInFormatters*)
    formatters.foreach { case (name, formatter) =>
      if (!formatterNames.add(name)) report(DuplicateFormatter(name))
      parameters(formatter.parameters, DuplicateFormatterParameter(name, _))
    }
    if (depthLimit < 1) report(InvalidDepthLimit(depthLimit))
    if (nodeLimit < 0 || characterLimit < 0) report(InvalidSizeLimit(nodeLimit, characterLimit))
    if (readBytes < 0 || readTime.isNegative) report(InvalidReadLimit(readBytes, readTime))
    errors.result().toList match {
      case Nil => Right(new PsiLoader(settings))
      case first :: rest => Left(PsiBuildErrors(::(first, rest)))
    }
  }
}

private[psi] object PsiBuilder {
  private val BuiltInMarkers: Seq[String] = Seq("<", ">", "width", "+")

  /** A run of the marker symbols, or a name by the key rule, as the syntax reads a marker's name (§5). */
  private def isMarkerName(name: String): Boolean =
    name.nonEmpty &&
      (name.forall(MarkerParser.isSymbol) ||
        Lexer.isNameStart(name.codePointAt(0)) && name.codePoints.allMatch(cp => Lexer.isNamePart(cp)))

  private[psi] val empty: PsiBuilder = new PsiBuilder(Settings())
}

/** An error in the configuration of a [[PsiBuilder]]. */
sealed trait PsiBuildError

/** A directive registered under a name already taken, the built-in `include`, `prop` and `env` included. */
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

/** A formatter registered under a name already taken, the built-in `join`'s included. */
final case class DuplicateFormatter(name: String) extends PsiBuildError

/** A formatter declaring a parameter twice. */
final case class DuplicateFormatterParameter(formatter: String, parameter: String) extends PsiBuildError

/** A depth limit below 1. */
final case class InvalidDepthLimit(limit: Int) extends PsiBuildError

/** A size limit below 0, of nodes or of characters. */
final case class InvalidSizeLimit(nodes: Int, characters: Int) extends PsiBuildError

/** A read limit below 0 bytes, or of a negative time. */
final case class InvalidReadLimit(bytes: Int, time: Duration) extends PsiBuildError

/** Every error of a [[PsiBuilder]]'s configuration, in the order the handlers were registered. */
final case class PsiBuildErrors(errors: ::[PsiBuildError])
