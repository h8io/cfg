package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.io.{FileNotFoundException, IOException}
import java.net.{JarURLConnection, MalformedURLException, URI, URISyntaxException, URL}
import java.nio.file.{Files, Paths}
import java.util.jar.JarFile

/** Why `#include` failed (`format.md` §6), carried by the [[DirectiveFailed]] at the call. */
sealed trait IncludeError extends DirectiveError

/** No target, or more than one: `targets` names the ones given, of `file`, `resource` and `url`. */
final case class IncludeTargets(targets: List[String]) extends IncludeError {
  def message: String = "exactly one of file, resource and url is required, given " + targets.mkString("[", ", ", "]")
}

/** `optional` neither `true` nor `false`. */
final case class IncludeOptional(value: String) extends IncludeError {
  def message: String = "optional is true or false, not " + value
}

/** A target that is no path or URL at all. */
final case class IncludeInvalidTarget(target: String) extends IncludeError {
  def message: String = target + " is no path or URL"
}

/** A target that is not there, and the include not optional. */
final case class IncludeNotFound(target: String) extends IncludeError {
  def message: String = target + " is not found"
}

/** A target that is there and cannot be read, optional or not. */
final case class IncludeUnreadable(target: String, cause: IOException) extends IncludeError {
  def message: String = target + " cannot be read: " + cause.toString
}

/** The built-in `#include` (`format.md` §6): a Psi source from a file, a resource or a URL, parsed and expanded where
  * the call stands.
  */
private[psi] object Include extends Directive {
  def name: String = "include"

  def parameters: Seq[Parameter] =
    Seq("file", "resource", "url", "optional").map(Parameter(_, required = false))

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Option[Fragment]] = {
    def argument(name: String): Option[String] =
      arguments(name) match {
        case Node.IScalar(_, _, value, _) => Some(value)
        case _ => None
      }
    val targets = List("file", "resource", "url").flatMap(kind => argument(kind).map(kind -> _))
    for {
      optional <- argument("optional") match {
        case None | Some("false") => Right(false)
        case Some("true") => Right(true)
        case Some(other) => Left(IncludeOptional(other))
      }
      target <- targets match {
        case target :: Nil => Right(target)
        case _ => Left(IncludeTargets(targets.map(_._1)))
      }
      candidates <- urls(target._1, target._2, context)
      fragment <- read(target._2, candidates, optional, context)
    } yield fragment
  }

  /** Where to look for the target, in order. */
  private def urls(kind: String, target: String, context: DirectiveContext): Either[IncludeError, List[URL]] =
    try
      kind match {
        case "file" => Right(extended(target).map(file(_, context.url)))
        case "resource" =>
          val loader = Option(Thread.currentThread.getContextClassLoader).getOrElse(getClass.getClassLoader)
          Right(extended(target).flatMap(name => Option(loader.getResource(name))))
        case _ => Right(URI.create(target).toURL :: Nil)
      }
    catch {
      // a path the file system cannot take, an `InvalidPathException`, is an `IllegalArgumentException`
      case _: MalformedURLException | _: URISyntaxException | _: IllegalArgumentException =>
        Left(IncludeInvalidTarget(target))
    }

  /** A path or a name with no extension, as each of Psi's; one with an extension, as it is. */
  private def extended(target: String): List[String] =
    if (target.substring(target.lastIndexOf('/') + 1).contains('.')) target :: Nil
    else List(".Ψ", ".ψ", ".psi").map(target + _)

  /** A path: absolute, a file; relative, against the including source's URL, or the working directory with none. */
  private def file(path: String, base: Option[URL]): URL =
    if (Paths.get(path).isAbsolute) Paths.get(path).toUri.toURL
    else base.fold(Paths.get(path).toAbsolutePath.toUri.toURL)(resolve(_, path))

  /** `path` against `base`, an entry of a jar against the entry's path: a `jar:` URI is opaque, and `URI.resolve`
    * cannot look into it.
    */
  private def resolve(base: URL, path: String): URL = {
    val relative = new URI(null, null, path, null)
    val text = base.toString
    val entry = text.indexOf("!/")
    (if (entry < 0) base.toURI.resolve(relative)
     else URI.create(text.substring(0, entry + 1) + URI.create(text.substring(entry + 1)).resolve(relative))).toURL
  }

  /** The first candidate there, parsed; nothing when none is and the include is optional. Whether a candidate is there
    * is asked before it is read: a file that cannot be read, its permissions denying it, throws what a missing one
    * does.
    */
  private def read(
      target: String,
      candidates: List[URL],
      optional: Boolean,
      context: DirectiveContext): Either[IncludeError, Option[Fragment]] =
    candidates match {
      case url :: rest =>
        present(target, url).flatMap { there =>
          if (there.contains(false)) read(target, rest, optional, context)
          else
            context.parse(target, url) match {
              case Right(fragment) => Right(Some(fragment))
              // a target known to be there only once read is not there when reading it finds nothing: a page not found
              case Left(_: FileNotFoundException) if there.isEmpty => read(target, rest, optional, context)
              case Left(cause) => Left(IncludeUnreadable(target, cause))
            }
        }
      case Nil => if (optional) Right(None) else Left(IncludeNotFound(target))
    }

  /** Whether `url` is there, as the file system says for a file and for an entry of a jar that is a file; none for any
    * other URL, which is known only once read.
    */
  private def present(target: String, url: URL): Either[IncludeError, Option[Boolean]] =
    try
      url.getProtocol match {
        case "file" => Right(Some(Files.exists(Paths.get(url.toURI))))
        case "jar" =>
          // opening no connection: a connection to a missing entry fails as one the jar cannot give
          val connection = url.openConnection().asInstanceOf[JarURLConnection]
          val archive = connection.getJarFileURL
          if (archive.getProtocol != "file") Right(None)
          else {
            val path = Paths.get(archive.toURI)
            if (!Files.exists(path)) Right(Some(false))
            else {
              val jar = new JarFile(path.toFile)
              try Right(Some(jar.getEntry(connection.getEntryName) != null))
              finally jar.close()
            }
          }
        case _ => Right(None)
      }
    catch {
      case cause: IOException => Left(IncludeUnreadable(target, cause))
      // a `file:` URL with a host, `file://host/x`, is no path
      case _: IllegalArgumentException => Left(IncludeInvalidTarget(target))
    }
}
