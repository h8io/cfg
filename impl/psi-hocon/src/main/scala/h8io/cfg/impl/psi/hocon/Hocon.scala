package h8io.cfg.impl.psi.hocon

import com.typesafe.config.ConfigException
import h8io.cfg.impl.hocon.HOCON
import h8io.cfg.impl.psi.*
import h8io.cfg.{Id, Node}

import java.io.FileNotFoundException

/** A HOCON file that cannot be read, parsed or resolved, `target` as written, with what Typesafe Config said of it. */
final case class HoconInvalid(target: String, reason: String) extends DirectiveError {
  def message: String = target + " cannot be read as HOCON: " + reason
}

/** Psi's import of a HOCON file (`format.md` §6), registered under a name of the loader's choosing, `hocon` by custom:
  * `Psi.builder.withDirective("hocon", Hocon)`.
  *
  * `#hocon(file: "app.conf")` gives the file's whole tree where the call stands, resolved on its own: no system
  * property, no environment variable and no `reference.conf` reach it. The target is written as `#include`'s, a file or
  * a resource with no extension looked for as `.conf`. Typesafe Config reads the file and those it includes itself,
  * past the load's read limit, and recurses with no limit: for trusted input only.
  */
object Hocon extends Directive {
  def parameters: Seq[Parameter] = Target.parameters

  /** The extensions a file or a resource with none is looked for with. */
  def Extensions: Seq[String] = Seq(".conf")

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
    context.target(arguments, Extensions).flatMap {
      case Some(target) =>
        try Right(Fragment(HOCON.isolated(target.url)))
        catch {
          // the target itself, a URL read from elsewhere, known not to be there only once read; not a file it includes
          case e: ConfigException.IO if missing(e, target) =>
            if (target.optional) Right(Fragment.None) else Left(IncludeNotFound(target.written))
          case e: ConfigException => Left(HoconInvalid(target.written, e.getMessage))
        }
      case None => Right(Fragment.None)
    }

  private def missing(e: ConfigException.IO, target: Target): Boolean =
    !target.known && e.getCause.isInstanceOf[FileNotFoundException] &&
      Option(e.origin).flatMap(o => Option(o.url)).contains(target.url)
}
