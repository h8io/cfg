package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

/** A formatter, registered with a loader under a name through [[PsiBuilder.withFormatter]]: what `\${path; name(…)}`
  * calls to turn the node a substitution finds into a string.
  *
  * It is called in two steps. First with its arguments, literals only, laid out over the [[parameters]] as a
  * directive's are, once the framework has found every argument going to a parameter, none twice and every required one
  * given; this happens when the source is read, so that what it refuses is reported with the errors of the syntax. Then
  * the function it gave is called with the node, resolved, when the substitution is: a scalar, a `null`, a map or a
  * sequence, at the position it is defined at. An optional substitution that finds nothing calls no function.
  *
  * The name is the loader's to give, as a directive's is: a handler's messages do not name it, the report putting the
  * name it was called by in front.
  */
trait Formatter {

  /** The parameters, in the order positional arguments go to them. */
  def parameters: Seq[Parameter]

  /** The function a call formats with, or an error, reported at the formatter's name as its failure.
    *
    * @param arguments
    *   the arguments by parameter, in the order the parameters are declared; every value a scalar string
    * @return
    *   the function from the node found to its text, or an error of its own, reported at the formatter's name
    */
  def apply(arguments: Node.IMap[Id.Root]): Either[FormatterError, Node.Some => Either[FormatterError, String]]
}

/** Why a [[Formatter]] could not take its arguments or format a node; a handler's own errors extend it. */
trait FormatterError {
  def message: String

  /** What to do about it, shown under the message; none unless the handler gives one. */
  def hint: Option[String] = None
}

/** What the built-in `join` formats is no sequence; it is defined at `found`. */
final case class JoinNotSequence(found: PsiLocation) extends FormatterError {
  def message: String = "what it joins is not a sequence; it is defined at " + found.description
}

/** An element of the sequence the built-in `join` formats, at `index`, is no scalar, `null` included; it is defined at
  * `found`.
  */
final case class JoinElementNotScalar(index: Int, found: PsiLocation) extends FormatterError {
  def message: String =
    "element " + index.toString + " of what it joins is not a scalar; it is defined at " + found.description
}
