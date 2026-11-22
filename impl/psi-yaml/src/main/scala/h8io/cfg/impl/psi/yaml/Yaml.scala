package h8io.cfg.impl.psi.yaml

import h8io.cfg.impl.psi.*
import h8io.cfg.impl.yaml.YAML
import h8io.cfg.{Id, Node}
import org.snakeyaml.engine.v2.exceptions.YamlEngineException

/** A YAML file at `source` that cannot be parsed as one, with what the parser said of it. */
final case class YamlInvalid(source: String, reason: String) extends DirectiveError {
  def message: String = source + " cannot be read as YAML: " + reason
}

/** Psi's import of a YAML file, registered under a name of the loader's choosing, `yaml` by custom:
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
    context.find(arguments, Extensions)(url => context.read(url).map(_.map(url -> _))).flatMap {
      case Some(Some((url, bytes))) =>
        try Right(Fragment(YAML.parse(url.toString, bytes)))
        catch { case e: YamlEngineException => Left(YamlInvalid(url.toString, e.getMessage)) }
      // the load's read limit gone, which the load reports, or an optional file not there
      case _ => Right(Fragment.None)
    }
}
