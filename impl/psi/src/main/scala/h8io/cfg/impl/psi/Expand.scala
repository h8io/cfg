package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.io.IOException
import java.net.URL
import scala.collection.{immutable, mutable}

/** Parses a source and expands its directives (`format.md` §6, §8), the sources they parse included, depth-first.
  *
  * A call is expanded only when its directive is registered and not disabled and its arguments go to its parameters;
  * any failure is reported at the call, which then contributes nothing, and expansion goes on. Every source being
  * expanded is on a stack, known by its key: parsing one already there is a cycle.
  */
private[psi] final class Expand private (directives: Map[String, Directive], disabled: Set[String]) {
  import Expand.*

  private val errors = mutable.ListBuffer.empty[PsiError]

  private def report(error: PsiError): Unit = {
    errors += error
    ()
  }

  /** `source` parsed and expanded, standing on `stack`, the sources that parsed it, the latest first. */
  private def document(source: Source, stack: List[Source]): Ast.Document = {
    val parsed = Parser.parse(source)
    errors ++= parsed.errors
    Ast.Document(statements(parsed.value.statements, source :: stack))
  }

  private def statements(statements: Vector[Ast.Statement], stack: List[Source]): Vector[Ast.Statement] =
    statements.flatMap {
      case field: Ast.Field => value(field.value, stack).map(value => field.copy(value = value))
      case Ast.DirectiveStatement(call, _) => run(call, stack).fold(Vector.empty[Ast.Statement])(fields(call, _))
    }

  private def value(value: Ast.Value, stack: List[Source]): Option[Ast.Value] =
    value match {
      case block: Ast.Block => Some(block.copy(statements = statements(block.statements, stack)))
      case sequence: Ast.Sequence =>
        val elements = sequence.elements.flatMap(e => this.value(e.value, stack).map(v => e.copy(value = v)))
        Some(sequence.copy(elements = elements))
      case Ast.DirectiveValue(call, tags) => run(call, stack).map(placed(call, tags, _))
      case _ => Some(value)
    }

  /** The fields of a fragment in statement position, which must be a map. */
  private def fields(call: Ast.Call, fragment: Fragment): Vector[Ast.Statement] =
    fragment match {
      case Fragment.Parsed(document, _) => document.statements
      case Fragment.Built(map: Node.IMap[?]) => statements(call, map)
      case Fragment.Built(_) =>
        report(DirectiveNotMap(call.location, call.name))
        Vector.empty
    }

  /** A fragment in value position, with the tags written before the call added to its own. */
  private def placed(call: Ast.Call, tags: Set[String], fragment: Fragment): Ast.Value =
    fragment match {
      case Fragment.Parsed(document, root) => Ast.Block(document.statements, tags, root)
      case Fragment.Built(node) => built(call, node, tags)
    }

  private def statements(call: Ast.Call, map: Node.IMap[?]): Vector[Ast.Statement] =
    map.iterator.map { child =>
      Ast.Field(::(Ast.Key(child.id.key, call.location), Nil), Ast.Operator.Merge, built(call, child, Set.empty), None)
    }.toVector

  /** `node` as a value of the syntax tree, its location kept as an origin behind the call. */
  private def built(call: Ast.Call, node: Node.ISome[Id], tags: Set[String]): Ast.Value = {
    val location = DirectiveLocation(call.location, call.name, node.location)
    node match {
      case Node.IScalar(_, own, value, _) => Ast.Scalar(value, own ++ tags, location)
      case Node.INull(_, own, _) => Ast.Null(own ++ tags, location)
      case map: Node.IMap[?] => Ast.Block(statements(call, map), map.tags ++ tags, location)
      case seq: Node.ISeq[?] =>
        Ast.Sequence(
          seq.iterator.map(e => Ast.Element(built(call, e, Set.empty), None)).toVector, seq.tags ++ tags, location)
    }
  }

  /** The fragment a call gives, none when it gives nothing or fails. */
  private def run(call: Ast.Call, stack: List[Source]): Option[Fragment] =
    if (disabled(call.name)) {
      report(DisabledDirective(call.location, call.name))
      None
    } else
      directives.get(call.name) match {
        case Some(directive) =>
          arguments(call, directive.parameters).flatMap { arguments =>
            directive(arguments, new Context(call, stack)) match {
              case Left(message) =>
                report(DirectiveFailed(call.location, call.name, message))
                None
              case Right(fragment) => fragment
            }
          }
        // the built-in one, plan step 11
        case None if call.name == "include" =>
          report(NotSupportedYet(call.location, "#include"))
          None
        case None =>
          report(UnknownDirective(call.location, call.name))
          None
      }

  /** The arguments of `call` by parameter — positional ones in order, then named ones — or none when one goes to no
    * parameter, or to one already given, or a required one is missing; every such argument reported.
    */
  private def arguments(call: Ast.Call, parameters: Seq[Parameter]): Option[Node.IMap[Id.Root]] = {
    val names = parameters.map(_.name).toVector
    val bound = mutable.LinkedHashMap.empty[String, Ast.Argument]
    val wrong = call.arguments.zipWithIndex.map {
      case (argument @ Ast.Argument(None, _, at, _), index) =>
        if (index < names.length) bind(bound, names(index), argument)
        else fail(ExtraArgument(at))
      case (argument @ Ast.Argument(Some(name), _, at, _), _) =>
        if (!names.contains(name)) fail(UnknownArgument(at, name))
        else if (bound.contains(name)) fail(DuplicateArgument(at, name))
        else bind(bound, name, argument)
    }
    val missing = parameters.filter(p => p.required && !bound.contains(p.name)).map { parameter =>
      fail(MissingArgument(call.location, parameter.name))
    }
    if (wrong.contains(false) || missing.nonEmpty) None
    else {
      val keys = names.filter(bound.contains)
      val entries = keys.map(key =>
        key -> (Tree.Scalar(bound(key).value, Set.empty, bound(key).valueLocation): Tree.Value))
      Some(MapImpl(Id.Root, Tree.Map(keys, immutable.Map(entries*), Set.empty, call.location)))
    }
  }

  private def bind(bound: mutable.Map[String, Ast.Argument], name: String, argument: Ast.Argument): Boolean = {
    bound(name) = argument
    true
  }

  private def fail(error: PsiError): Boolean = {
    report(error)
    false
  }

  private final class Context(call: Ast.Call, stack: List[Source]) extends DirectiveContext {
    def location: SourceLocation = call.location

    def source: String = stack.head.name

    def url: Option[URL] = stack.head.url

    def parse(name: String, url: URL): Either[IOException, Fragment] =
      Source.read(name, url) match {
        case Right(source) => Right(nested(source))
        case Left(SourceUnreadable(_, cause)) => Left(cause)
        case Left(error) =>
          report(error)
          Right(Fragment.Parsed(Ast.Document(Vector.empty), SourceLocation(name, 1, 1)))
      }

    def parse(name: String, text: String): Fragment = nested(Source(name, text))

    private def nested(source: Source): Fragment = {
      val root = SourceLocation(source.name, 1, 1)
      if (stack.exists(_.key == source.key)) {
        val chain = (source :: stack).reverse.map(_.name)
        report(DirectiveCycle(call.location, ::(chain.head, chain.tail)))
        Fragment.Parsed(Ast.Document(Vector.empty), root)
      } else if (stack.length >= MaxDepth) {
        report(DirectiveTooDeep(call.location, MaxDepth))
        Fragment.Parsed(Ast.Document(Vector.empty), root)
      } else Fragment.Parsed(document(source, stack), root)
    }
  }
}

private[psi] object Expand {

  /** How many sources deep directives may parse, a backstop for a key that changes on every step (§6). */
  def MaxDepth: Int = 64

  /** `source` parsed and its directives expanded, with every error found in it and in the sources it parsed. */
  def apply(source: Source, directives: Map[String, Directive], disabled: Set[String]): Result[Ast.Document] = {
    val expand = new Expand(directives, disabled)
    val document = expand.document(source, Nil)
    Result(document, expand.errors.toList)
  }
}
