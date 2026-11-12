package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.io.IOException
import java.net.URL

/** A directive's handler, registered with a loader through [[PsiBuilder.withDirectives]] (`format.md` §6).
  *
  * A call `#name(…)` is expanded while its source is read, before any merging: its arguments, literals only, are laid
  * out over the [[parameters]] — positional ones in order, then named ones — and the handler is called only when every
  * argument goes to a parameter, none twice, and every required one is given; the framework reports anything else.
  */
trait Directive {

  /** The name the directive is called by, `#name`. */
  def name: String

  /** The parameters, in the order positional arguments go to them. */
  def parameters: Seq[Parameter]

  /** Expands a call.
    *
    * @param arguments
    *   the arguments by parameter, in the order the parameters are declared; every value a scalar string
    * @return
    *   a message, reported as the directive's failure at the call; nothing, which adds no field in statement position
    *   and drops the field or element in value position; or a fragment, which must be a map in statement position
    */
  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[String, Option[Fragment]]
}

/** A parameter of a [[Directive]]: an argument for a required one must be given. */
final case class Parameter(name: String, required: Boolean)

/** What a [[Directive]] is given besides its arguments. */
trait DirectiveContext {

  /** Where the call stands: its `#`. */
  def location: SourceLocation

  /** The name of the source the call stands in, as locations name it (§10). */
  def source: String

  /** The URL of the source the call stands in, none for a source given as text. */
  def url: Option[URL]

  /** Reads and parses the source at `url`, named `name` in locations, its own directives expanded. A source that cannot
    * be read is handed back; every other error in it, and a cycle — parsing a source already being expanded — goes with
    * the fragment, which holds what was read, and is reported where the fragment is used.
    */
  def parse(name: String, url: URL): Either[IOException, Fragment]

  /** Parses a source held as `text`, named `name` in locations and known by its name for cycles. */
  def parse(name: String, text: String): Fragment
}

/** What a [[Directive]] gives back: Psi's own unexpanded tree, opaque to the handler. A fragment is a value: it carries
  * the errors found in making it, reported wherever it is used, so a handler may keep one and give it back again, in
  * the same load or a later one.
  */
sealed abstract class Fragment

object Fragment {

  /** A fragment of `node`, which holds no substitution; its ids are dropped, and its locations are kept as origins
    * behind the call (§10).
    */
  def apply(node: Node.ISome[Id]): Fragment = Built(node)

  /** A source parsed, its directives expanded; `root` is where it starts, `errors` what was found in it, reported
    * wherever the fragment is used.
    */
  private[psi] final case class Parsed(document: Ast.Document, root: SourceLocation, errors: List[PsiError])
      extends Fragment

  private[psi] final case class Built(node: Node.ISome[Id]) extends Fragment
}
