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

  /** The target `arguments` name as `#include`'s do (`format.md` §6), by [[Target.parameters]], opened by `open`:
    * exactly one of `file`, `resource` and `url`, a file relative to the source the call stands in, and `optional`. A
    * file or a resource with no extension is looked for with each of `extensions` in turn, `.yaml` for one, and each
    * candidate is opened in turn, what `open` gives for the first there its result. A candidate known not to be there,
    * a file, is not opened; one not known to be, a URL read from elsewhere, is not there when `open` fails as a
    * `FileNotFoundException`, and the next is opened. None when no candidate is there and the target is optional;
    * `#include`'s error otherwise, a candidate `open` fails on otherwise unreadable.
    */
  def find[A](arguments: Node.IMap[Id.Root], extensions: Seq[String])(
      open: URL => Either[IOException, A]): Either[DirectiveError, Option[A]]

  /** The bytes at `url`, read with `#include`'s timeouts and counted against the load's read limit (§11); none once the
    * limit is gone, by this read or one before it, the load reporting the limit itself. A URL that is not there fails
    * as a `FileNotFoundException`.
    */
  def read(url: URL): Either[IOException, Option[Array[Byte]]]
}

/** How a target is written, for a handler that takes one as `#include` does ([[DirectiveContext.find]]). */
object Target {

  /** The parameters a target is written with, as `#include`'s: `file`, `resource`, `url` and `optional`, none required.
    */
  val parameters: Seq[Parameter] = Seq("file", "resource", "url", "optional").map(Parameter(_, required = false))
}
