package h8io.cfg.impl.psi

/** A parameter of a [[Directive]] or a [[Marker]]: an argument for a required one must be given. */
final case class Parameter(name: String, required: Boolean)
