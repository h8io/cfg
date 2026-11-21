package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.io.{FileNotFoundException, IOException}
import java.net.{JarURLConnection, MalformedURLException, URI, URISyntaxException, URL}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

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

  def parameters: Seq[Parameter] = Target.parameters

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
    find(arguments, Extensions, context.url)(context.parse).map(_.getOrElse(Fragment.None))

  /** Psi's extensions, in the order a file or a resource with none is looked for with them (§6). */
  private val Extensions: Seq[String] = Seq(".Ψ", ".ψ", ".psi")

  /** The target `arguments` name, opened by `open` (`DirectiveContext.find`): the first candidate it opens, a file or a
    * resource with no extension looked for with each of `extensions`, a relative file against `base`; none when no
    * candidate is there and the target is optional.
    */
  def find[A](arguments: Node.IMap[Id.Root], extensions: Seq[String], base: Option[URL])(
      open: URL => Either[IOException, A]): Either[IncludeError, Option[A]] =
    for {
      written <- this.written(arguments)
      candidates <- urls(written._1, written._2, extensions, base)
      found <- opened(written._2, candidates, written._3)(open)
    } yield found

  /** The kind of target `arguments` give, the target as written, and whether it is optional. */
  private def written(arguments: Node.IMap[Id.Root]): Either[IncludeError, (String, String, Boolean)] = {
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
    } yield (target._1, target._2, optional)
  }

  /** Where to look for the target, in order, a file or a resource with no extension with each of `extensions`. */
  private def urls(
      kind: String,
      target: String,
      extensions: Seq[String],
      base: Option[URL]): Either[IncludeError, List[URL]] =
    try
      kind match {
        // the last segment of a path as the file system the program runs on reads it
        case "file" =>
          Right(extended(target, Option(Paths.get(target).getFileName).exists(_.toString.contains('.')), extensions)
            .map(file(_, base)))
        case "resource" =>
          val loader = Option(Thread.currentThread.getContextClassLoader).getOrElse(getClass.getClassLoader)
          // a name on the class path separates its segments with `/` on every system
          Right(extended(target, target.substring(target.lastIndexOf('/') + 1).contains('.'), extensions)
            .flatMap(name => Option(loader.getResource(name))))
        case _ => Right(URI.create(target).toURL :: Nil)
      }
    catch {
      // a path the file system cannot take, an `InvalidPathException`, is an `IllegalArgumentException`
      case _: MalformedURLException | _: URISyntaxException | _: IllegalArgumentException =>
        Left(IncludeInvalidTarget(target))
    }

  /** A path or a name with no extension, with each of `extensions`; one with an extension, as it is. */
  private def extended(target: String, extension: Boolean, extensions: Seq[String]): List[String] =
    if (extension) target :: Nil
    else extensions.toList.map(target + _)

  /** A path: absolute, a file; relative, against the including source's URL, or the working directory with none. */
  private def file(path: String, base: Option[URL]): URL =
    if (Paths.get(path).isAbsolute) Paths.get(path).toUri.toURL
    else base.fold(Paths.get(path).toAbsolutePath.toUri.toURL)(resolve(_, path))

  /** `path` against `base`, an entry of a jar against the entry's path: a `jar:` URI is opaque, and `URI.resolve`
    * cannot look into it. A colon in the first segment would read as a scheme's end, `part:one.psi` as the scheme
    * `part`, so such a path is read from `./`.
    */
  private def resolve(base: URL, path: String): URL = {
    val relative = new URI(null, null, if (path.takeWhile(_ != '/').contains(':')) "./" + path else path, null)
    val text = base.toString
    val entry = text.indexOf("!/")
    (if (entry < 0) base.toURI.resolve(relative)
     else URI.create(text.substring(0, entry + 1) + URI.create(text.substring(entry + 1)).resolve(relative))).toURL
  }

  /** The first candidate there, parsed; nothing when none is and the include is optional. Whether a candidate is there
    * is asked before it is read: a file that cannot be read, its permissions denying it, throws what a missing one
    * does.
    */
  @tailrec private def opened[A](target: String, candidates: List[URL], optional: Boolean)(
      open: URL => Either[IOException, A]): Either[IncludeError, Option[A]] =
    candidates match {
      case url :: rest =>
        present(target, url) match {
          case Right(Some(false)) => opened(target, rest, optional)(open)
          case Right(there) =>
            open(url) match {
              case Right(found) => Right(Some(found))
              // a target known to be there only once read is not there when reading it finds nothing: a page not found
              case Left(_: FileNotFoundException) if there.isEmpty => opened(target, rest, optional)(open)
              case Left(cause) => Left(IncludeUnreadable(target, cause))
            }
          case Left(error) => Left(error)
        }
      case Nil => if (optional) Right(None) else Left(IncludeNotFound(target))
    }

  /** Whether `url` is there, as the file system says for a file and for an entry of a jar that is a file; none for any
    * other URL, which is known only once read. A file is missing only when the file system says it is not there: one it
    * cannot tell about, its directory denying access, is taken as there, and reading it fails as unreadable.
    */
  private def present(target: String, url: URL): Either[IncludeError, Option[Boolean]] =
    try
      url.getProtocol match {
        case "file" => Right(Some(!Files.notExists(Source.path(url))))
        case "jar" =>
          // opening no connection: a connection to a missing entry fails as one the jar cannot give
          val connection = url.openConnection().asInstanceOf[JarURLConnection]
          val archive = connection.getJarFileURL
          if (archive.getProtocol != "file") Right(None)
          else {
            val path = Source.path(archive)
            if (Files.notExists(path)) Right(Some(false))
            // the jar itself, which is there and is no source: reading it fails as unreadable
            else if (connection.getEntryName == null) Right(Some(true))
            else {
              val jar = Source.open(path.toFile, url)
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
