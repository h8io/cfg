package h8io.cfg.impl.psi

/** A handler's error in tests, for directives and markers alike. */
final case class Problem(message: String, override val hint: Option[String] = None)
    extends DirectiveError with MarkerError
