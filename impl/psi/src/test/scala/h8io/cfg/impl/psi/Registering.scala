package h8io.cfg.impl.psi

/** Registers handlers with a builder in turn, each under its name. */
object Registering {
  implicit final class Registrations(private val builder: PsiBuilder) extends AnyVal {
    def withDirectives(directives: (String, Directive)*): PsiBuilder =
      directives.foldLeft(builder) { case (built, (name, directive)) => built.withDirective(name, directive) }

    def withMarkers(markers: (String, Marker)*): PsiBuilder =
      markers.foldLeft(builder) { case (built, (name, marker)) => built.withMarker(name, marker) }

    def withFormatters(formatters: (String, Formatter)*): PsiBuilder =
      formatters.foldLeft(builder) { case (built, (name, formatter)) => built.withFormatter(name, formatter) }
  }
}
