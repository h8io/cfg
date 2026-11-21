package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

/** A marker of a triple-quoted block, registered with a loader under a name through [[PsiBuilder.withMarker]]
  * (`format.md` §5): a function from the block's value to a new one, given its arguments. The loader's markers on a
  * block run left to right, after the built-in ones, over the value with its escapes read and its substitutions
  * resolved. The name is the loader's to give, as a directive's is.
  */
trait Marker {

  /** The parameters, in the order positional arguments go to them. */
  def parameters: Seq[Parameter]

  /** The new value of a block, or an error, reported at the marker as its failure.
    *
    * @param arguments
    *   the arguments by parameter, in the order the parameters are declared; every value a scalar string
    */
  def apply(arguments: Node.IMap[Id.Root], value: String): Either[MarkerError, String]
}

/** Why a [[Marker]] could not give a new value; a handler's own errors extend it. */
trait MarkerError {
  def message: String

  /** What to do about it, shown under the message; none unless the handler gives one. */
  def hint: Option[String] = None
}
