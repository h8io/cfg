package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.io.IOException
import java.net.URL

/** A directive's handler, registered with a loader under a name through [[PsiBuilder.withDirective]] (`format.md` §6).
  * The name is the loader's to give, not the handler's: its own messages do not name it, the report putting the name it
  * was called by in front.
  *
  * A call `#name(…)` is expanded while its source is read, before any merging: its arguments, literals only, are laid
  * out over the [[parameters]] — positional ones in order, then named ones — and the handler is called only when every
  * argument goes to a parameter, none twice, and every required one is given; the framework reports anything else.
  */
trait Directive {

  /** The parameters, in the order positional arguments go to them. */
  def parameters: Seq[Parameter]

  /** Expands a call.
    *
    * @param arguments
    *   the arguments by parameter, in the order the parameters are declared; every value a scalar string
    * @return
    *   an error, reported as the directive's failure at the call; [[Fragment.None]], nothing, which adds no field in
    *   statement position and drops the field or element in value position; or a fragment, which must be a map in
    *   statement position
    */
  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment]
}

/** Why a [[Directive]] could not be expanded; a handler's own errors extend it. */
trait DirectiveError {
  def message: String

  /** What to do about it, shown under the message; none unless the handler gives one. */
  def hint: Option[String] = None
}

/** What a [[Directive]] is given besides its arguments. */
trait DirectiveContext {

  /** Where the call stands: its `#`. */
  def location: SourceLocation

  /** The name of the source the call stands in, as locations name it (§10). */
  def source: String

  /** The URL of the source the call stands in, none for a source given as text. */
  def url: Option[URL]

  /** Reads and parses the source at `url`, named by the URL in locations, its own directives expanded. A source that
    * cannot be read is handed back; every other error in it, and a cycle — parsing a source already being expanded —
    * goes with the fragment, which holds what was read, and is reported where the fragment is used. Once the load's
    * read limit is gone, by this read or one before it, the fragment is empty: the load reports the limit itself.
    */
  def parse(url: URL): Either[IOException, Fragment]

  /** Parses a source held as `text`, named `name` in locations and known by its name for cycles. */
  def parse(name: String, text: String): Fragment
}
