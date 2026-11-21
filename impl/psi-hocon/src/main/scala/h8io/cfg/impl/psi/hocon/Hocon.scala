package h8io.cfg.impl.psi.hocon

import com.typesafe.config.ConfigException
import h8io.cfg.impl.hocon.HOCON
import h8io.cfg.impl.psi.*
import h8io.cfg.{Id, Node}

import java.io.{FileNotFoundException, IOException}
import java.net.URL

/** A HOCON file at `source` that cannot be read, parsed or resolved, with what Typesafe Config said of it. */
final case class HoconInvalid(source: String, reason: String) extends DirectiveError {
  def message: String = source + " cannot be read as HOCON: " + reason
}

/** Psi's import of a HOCON file (`format.md` §6), registered under a name of the loader's choosing, `hocon` by custom:
  * `Psi.builder.withDirective("hocon", Hocon)`.
  *
  * `#hocon(file: "app.conf")` gives the file's whole tree where the call stands, resolved on its own: no system
  * property, no environment variable and no `reference.conf` reach it. The target is written as `#include`'s, a file or
  * a resource with no extension looked for as `.conf`, `.json` and `.properties`, in that order. Typesafe Config reads
  * the file and those it includes itself, past the load's read limit, and recurses with no limit: for trusted input
  * only.
  */
object Hocon extends Directive {
  def parameters: Seq[Parameter] = Target.parameters

  /** The extensions a file or a resource with none is looked for with, in order, the first found read: each a syntax
    * HOCON takes, as Typesafe Config's own, which merges every one it finds.
    */
  val Extensions: Seq[String] = Seq(".conf", ".json", ".properties")

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
    context.find(arguments, Extensions)(load).flatMap {
      case Some(loaded) => loaded.map(Fragment(_))
      case None => Right(Fragment.None)
    }

  /** The file at `url`, or its error; not there, as the context takes it, when Typesafe Config finds the file itself
    * missing, not one it includes.
    */
  private def load(url: URL): Either[IOException, Either[DirectiveError, Node.IMap[Id.Root]]] =
    try Right(Right(HOCON.isolated(url)))
    catch {
      case e: ConfigException.IO if missing(e, url) => Left(new FileNotFoundException(url.toString))
      case e: ConfigException => Right(Left(HoconInvalid(url.toString, e.getMessage)))
    }

  private def missing(e: ConfigException.IO, url: URL): Boolean =
    e.getCause.isInstanceOf[FileNotFoundException] && Option(e.origin).flatMap(o => Option(o.url)).contains(url)
}
