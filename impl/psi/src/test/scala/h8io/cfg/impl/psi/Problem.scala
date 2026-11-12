package h8io.cfg.impl.psi

/** A handler's error in tests, for directives and markers alike. */
final case class Problem(message: String) extends DirectiveError with MarkerError
