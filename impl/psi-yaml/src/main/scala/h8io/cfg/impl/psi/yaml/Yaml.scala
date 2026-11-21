package h8io.cfg.impl.psi.yaml

import h8io.cfg.impl.psi.*
import h8io.cfg.impl.yaml.YAML
import h8io.cfg.{Id, Node}
import org.snakeyaml.engine.v2.exceptions.YamlEngineException

import java.io.FileNotFoundException

/** A YAML file that cannot be parsed as one, `target` as written, with what the parser said of it. */
final case class YamlInvalid(target: String, reason: String) extends DirectiveError {
  def message: String = target + " cannot be read as YAML: " + reason
}

/** Psi's import of a YAML file (`format.md` §6), registered under a name of the loader's choosing, `yaml` by custom:
  * `Psi.builder.withDirective("yaml", Yaml)`.
  *
  * `#yaml(file: "db.yaml")` gives the file's whole tree where the call stands, its root any node, its tags the tags
  * written: the target written as `#include`'s, a file or a resource with no extension looked for as `.yaml` and then
  * `.yml`, its bytes read through the load's read limit.
  */
object Yaml extends Directive {
  def parameters: Seq[Parameter] = Target.parameters

  /** The extensions a file or a resource with none is looked for with, in order. */
  val Extensions: Seq[String] = Seq(".yaml", ".yml")

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
    context.target(arguments, Extensions).flatMap {
      case Some(target) =>
        context.read(target.url) match {
          case Right(Some(bytes)) =>
            try Right(Fragment(YAML.parse(target.url.toString, bytes)))
            catch { case e: YamlEngineException => Left(YamlInvalid(target.written, e.getMessage)) }
          // the load's read limit gone, which the load reports
          case Right(None) => Right(Fragment.None)
          // a URL read from elsewhere, known not to be there only once read
          case Left(_: FileNotFoundException) if !target.known =>
            if (target.optional) Right(Fragment.None) else Left(IncludeNotFound(target.written))
          case Left(cause) => Left(IncludeUnreadable(target.written, cause))
        }
      case None => Right(Fragment.None)
    }
}
