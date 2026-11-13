package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}

import java.io.IOException
import java.net.URL
import scala.collection.{immutable, mutable}
import scala.util.control.ControlThrowable

/** Parses a source and expands its directives (`format.md` §6, §8), the sources they parse included, depth-first.
  *
  * A call is expanded only when its directive is registered and not disabled and its arguments go to its parameters;
  * any failure is reported at the call, which then contributes nothing, and expansion goes on. Every source being
  * expanded is on a stack, known by its key: parsing one already there is a cycle.
  *
  * A source a handler parses is expanded on its own, and its errors travel with the fragment, reported where the
  * fragment is used: a fragment is a value a handler may keep and give back again, in this load or a later one.
  */
private[psi] final class Expand private (
    directives: Map[String, Directive],
    disabled: Set[String],
    markers: Map[String, Marker],
    limit: Int) {
  import Expand.*

  private val errors = mutable.ListBuffer.empty[PsiError]

  /** The names of the sources whose fragments were used, in the order parsed, nested ones after theirs. */
  private val included = mutable.ListBuffer.empty[String]

  private def report(error: PsiError): Unit = {
    errors += error
    ()
  }

  /** `source` parsed and expanded `base` levels deep, standing on `stack`, the sources that parsed it, the latest
    * first.
    */
  private def document(source: Source, stack: List[Source], base: Int): Ast.Document = {
    val parsed = Parser.parse(source, markers, base, limit)
    errors ++= parsed.errors
    Ast.Document(statements(parsed.value.statements, source :: stack, base))
  }

  /** `statements` expanded, standing `depth` levels deep. */
  private def statements(statements: Vector[Ast.Statement], stack: List[Source], depth: Int): Vector[Ast.Statement] =
    statements.flatMap {
      case field: Ast.Field =>
        value(field.value, stack, depth + field.keys.size - 1).map(value => field.copy(value = value))
      case Ast.DirectiveStatement(call, _) =>
        run(call, stack, depth).fold(Vector.empty[Ast.Statement])(fields(call, _, depth))
    }

  /** `value` expanded, with `depth` levels around it. */
  private def value(value: Ast.Value, stack: List[Source], depth: Int): Option[Ast.Value] =
    value match {
      case block: Ast.Block => Some(block.copy(statements = statements(block.statements, stack, depth + 1)))
      case sequence: Ast.Sequence =>
        val elements =
          sequence.elements.flatMap(e => this.value(e.value, stack, depth + 1).map(v => e.copy(value = v)))
        Some(sequence.copy(elements = elements))
      case Ast.DirectiveValue(call, tags) => run(call, stack, depth + 1).flatMap(placed(call, tags, _, depth))
      case _ => Some(value)
    }

  /** The fields of a fragment in statement position `depth` levels deep, which must be a map and fit there. */
  private def fields(call: Ast.Call, fragment: Fragment, depth: Int): Vector[Ast.Statement] =
    fragment match {
      case Fragment.Parsed(document, _, errors, sources) =>
        this.errors ++= errors
        included ++= sources
        if (fits(call, depth + levels(document.statements))) document.statements else Vector.empty
      case Fragment.Built(map: Node.IMap[?]) => fitting(call)(statements(call, map, depth)).getOrElse(Vector.empty)
      case Fragment.Built(_) =>
        report(DirectiveNotMap(call.location, call.name))
        Vector.empty
    }

  /** A fragment in value position with `depth` levels around it, with the tags written before the call added to its
    * own; none when it does not fit there.
    */
  private def placed(call: Ast.Call, tags: Set[String], fragment: Fragment, depth: Int): Option[Ast.Value] =
    fragment match {
      case Fragment.Parsed(document, root, errors, sources) =>
        this.errors ++= errors
        included ++= sources
        if (fits(call, depth + 1 + levels(document.statements))) Some(Ast.Block(document.statements, tags, root))
        else None
      case Fragment.Built(node) => fitting(call)(built(call, node, tags, depth))
    }

  /** Whether a fragment reaching `depth` levels fits under the limit; reported at the call when it does not. */
  private def fits(call: Ast.Call, depth: Int): Boolean =
    depth <= limit || {
      report(DepthExceeded(call.location, limit, fatal = false))
      false
    }

  /** `build`, or none when what it builds goes past the limit, which is reported at the call. */
  private def fitting[A](call: Ast.Call)(build: => A): Option[A] =
    try Some(build)
    catch {
      case TooDeep =>
        report(DepthExceeded(call.location, limit, fatal = false))
        None
    }

  /** The fields of `map` standing `depth` levels deep. */
  private def statements(call: Ast.Call, map: Node.IMap[?], depth: Int): Vector[Ast.Statement] =
    map.iterator.map { child =>
      val value = built(call, child, Set.empty, depth)
      Ast.Field(::(Ast.Key(child.id.key, call.location), Nil), Ast.Operator.Merge, value, None)
    }.toVector

  /** `node` as a value of the syntax tree with `depth` levels around it, its location kept as an origin behind the
    * call; a container past the limit throws [[TooDeep]].
    */
  private def built(call: Ast.Call, node: Node.ISome[Id], tags: Set[String], depth: Int): Ast.Value = {
    val location = DirectiveLocation(call.location, call.name, node.location)
    node match {
      case Node.IScalar(_, own, value, _) => Ast.Scalar(value, own ++ tags, location)
      case Node.INull(_, own, _) => Ast.Null(own ++ tags, location)
      case map: Node.IMap[?] =>
        if (depth >= limit) throw TooDeep
        Ast.Block(statements(call, map, depth + 1), map.tags ++ tags, location)
      case seq: Node.ISeq[?] =>
        if (depth >= limit) throw TooDeep
        val elements = seq.iterator.map(e => Ast.Element(built(call, e, Set.empty, depth + 1), None)).toVector
        Ast.Sequence(elements, seq.tags ++ tags, location)
    }
  }

  /** The fragment a call gives, none when it gives nothing or fails; a source it parses stands `base` levels deep. */
  private def run(call: Ast.Call, stack: List[Source], base: Int): Option[Fragment] =
    if (disabled(call.name)) {
      report(DisabledDirective(call.location, call.name))
      None
    } else
      directives.get(call.name) match {
        case Some(directive) =>
          arguments(call, directive.parameters).flatMap { arguments =>
            directive(arguments, new Context(call, stack, base)) match {
              case Left(error) =>
                report(DirectiveFailed(call.location, call.name, error))
                None
              case Right(fragment) => fragment
            }
          }
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

  private final class Context(call: Ast.Call, stack: List[Source], base: Int) extends DirectiveContext {
    def location: SourceLocation = call.location

    def source: String = stack.head.name

    def url: Option[URL] = stack.head.url

    def parse(url: URL): Either[IOException, Fragment] =
      Source.read(url.toString, url) match {
        case Right(source) => Right(nested(source))
        case Left(SourceUnreadable(_, cause)) => Left(cause)
        case Left(error) =>
          Right(Fragment.Parsed(Ast.Document(Vector.empty), SourceLocation(url.toString, 1, 1), error :: Nil, Nil))
      }

    def parse(name: String, text: String): Fragment = nested(Source(name, text))

    private def nested(source: Source): Fragment = {
      val root = SourceLocation(source.name, 1, 1)
      if (stack.exists(_.key == source.key)) {
        val chain = (source :: stack).reverse.map(_.name)
        Fragment.Parsed(
          Ast.Document(Vector.empty), root, DirectiveCycle(call.location, ::(chain.head, chain.tail)) :: Nil, Nil)
      } else if (stack.length >= MaxDepth)
        Fragment.Parsed(Ast.Document(Vector.empty), root, DirectiveTooDeep(call.location, MaxDepth) :: Nil, Nil)
      else {
        val expand = new Expand(directives, disabled, markers, limit)
        val document = expand.document(source, stack, base)
        Fragment.Parsed(document, root, expand.errors.toList, source.name :: expand.included.toList)
      }
    }
  }
}

private[psi] object Expand {

  /** How many sources deep directives may parse, a backstop for a key that changes on every step (§6). */
  def MaxDepth: Int = 64

  /** A node a handler built that goes past the depth limit. */
  private case object TooDeep extends ControlThrowable

  /** How many levels `statements` open below where they stand, a dotted key's keys after the first counted. */
  private def levels(statements: Vector[Ast.Statement]): Int =
    // an expanded document holds no directive
    statements.iterator.collect { case field: Ast.Field => field.keys.size - 1 + height(field.value) }.foldLeft(0)(
      math.max)

  /** How many levels `value` opens. */
  private def height(value: Ast.Value): Int =
    value match {
      case Ast.Block(statements, _, _) => 1 + levels(statements)
      case Ast.Sequence(elements, _, _) => 1 + elements.iterator.map(e => height(e.value)).foldLeft(0)(math.max)
      case _ => 0
    }

  /** `source` parsed and its directives expanded, with every error found in it and in the sources it parsed, and the
    * names of those sources, in the order parsed.
    */
  def apply(
      source: Source,
      directives: Map[String, Directive],
      disabled: Set[String],
      markers: Map[String, Marker],
      limit: Int): Result[(Ast.Document, List[String])] = {
    val expand = new Expand(directives, disabled, markers, limit)
    val document = expand.document(source, Nil, 0)
    // a fragment used more than once, or two of one source, report the same errors: each is reported once
    Result((document, expand.included.toList), expand.errors.toList.distinct)
  }
}
