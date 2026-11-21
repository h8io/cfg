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
    formatters: Map[String, Formatter],
    limit: Int,
    budget: ReadBudget,
    nodes: Expand.Nodes) {
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
  private def document(source: Source, stack: List[Source], base: Int): AST.Document = {
    val parsed = Parser.parse(source, markers, base, limit, formatters)
    errors ++= parsed.errors
    AST.Document(statements(parsed.value.statements, source :: stack, base))
  }

  /** `statements` expanded, standing `depth` levels deep. */
  private def statements(statements: Vector[AST.Statement], stack: List[Source], depth: Int): Vector[AST.Statement] =
    statements.flatMap {
      case injection: AST.InjectionStatement => Vector(injection)
      case field: AST.Field =>
        value(field.value, stack, depth + field.keys.size - 1).map(value => field.copy(value = value))
      case AST.DirectiveStatement(call, _) =>
        run(call, stack, depth).fold(Vector.empty[AST.Statement])(fields(call, _, depth))
    }

  /** `value` expanded, with `depth` levels around it. */
  private def value(value: AST.Value, stack: List[Source], depth: Int): Option[AST.Value] =
    value match {
      case block: AST.Block => Some(block.copy(statements = statements(block.statements, stack, depth + 1)))
      case sequence: AST.Sequence =>
        val elements = sequence.elements.flatMap { e =>
          val values = (e.value, e.put) match {
            case (AST.DirectiveValue(call, _), AST.Put.Elements) =>
              run(call, stack, depth + 2).fold(Vector.empty[AST.Value])(injected(call, _, depth + 1))
            case (AST.DirectiveValue(call, tag), AST.Put.Found) =>
              run(call, stack, depth + 2).fold(Vector.empty[AST.Value])(spread(call, tag, _, depth + 1))
            case (other, _) => this.value(other, stack, depth + 1).toVector
          }
          values.map(v => e.copy(value = v))
        }
        Some(sequence.copy(elements = elements))
      case AST.DirectiveValue(call, tag) =>
        run(call, stack, depth + 1).fold(Option.empty[AST.Value])(placed(call, tag, _, depth))
      // what a call gives is kept, to be put once substitutions are resolved (§9)
      case AST.Injections(items, tag, location) =>
        val expanded = items.flatMap {
          case AST.Injected.Calling(call) =>
            run(call, stack, depth + 1)
              .fold(Option.empty[AST.Value])(placed(call, None, _, depth))
              .map(AST.Injected.Given(call, _))
          case other => Some(other)
        }
        if (expanded.isEmpty) None else Some(AST.Injections(expanded, tag, location))
      case _ => Some(value)
    }

  /** The fields of a fragment in statement position `depth` levels deep, which must be a map and fit there. */
  private def fields(call: AST.Call, fragment: Fragment.Some, depth: Int): Vector[AST.Statement] =
    fragment match {
      case parsed @ Fragment.Parsed(document, _, errors, sources) =>
        this.errors ++= errors
        included ++= sources
        if (fits(call, depth + levels(document.statements)) && fitting(call)(nodes.use(parsed)).nonEmpty)
          document.statements
        else Vector.empty
      case Fragment.Built(map: Node.IMap[?]) => fitting(call)(statements(call, map, depth)).getOrElse(Vector.empty)
      case Fragment.Built(_) =>
        report(DirectiveNotMap(call.location, call.name))
        Vector.empty
    }

  /** A fragment in value position with `depth` levels around it, with the tag written before the call in place of its
    * own; none when it does not fit there.
    */
  private def placed(call: AST.Call, tag: Option[String], fragment: Fragment.Some, depth: Int): Option[AST.Value] =
    fragment match {
      case parsed @ Fragment.Parsed(document, root, errors, sources) =>
        this.errors ++= errors
        included ++= sources
        if (fits(call, depth + 1 + levels(document.statements)) && fitting(call)(nodes.use(parsed)).nonEmpty)
          Some(AST.Block(document.statements, tag, root))
        else None
      case Fragment.Built(node) => fitting(call)(built(call, node, tag, depth))
    }

  /** A fragment injected among the elements of a sequence, `depth` levels deep: the elements of a sequence, in its
    * place; anything else is an error (§9).
    */
  private def injected(call: AST.Call, fragment: Fragment.Some, depth: Int): Vector[AST.Value] =
    fragment match {
      case Fragment.Built(seq: Node.ISeq[?]) =>
        fitting(call)(seq.iterator.map(built(call, _, None, depth)).toVector).getOrElse(Vector.empty)
      // a source parsed is a map
      case _ =>
        report(DirectiveNotSequence(call.location, call.name))
        Vector.empty
    }

  /** A fragment in `[…]`, `depth` levels deep: the elements of a sequence, in its place, or anything else as one
    * element, with the tag written before the call, which a sequence's elements have no place for (§9).
    */
  private def spread(call: AST.Call, tag: Option[String], fragment: Fragment.Some, depth: Int): Vector[AST.Value] =
    fragment match {
      case Fragment.Built(_: Node.ISeq[?]) if tag.nonEmpty =>
        report(TaggedDirectiveSequence(call.location, call.name))
        Vector.empty
      case Fragment.Built(_: Node.ISeq[?]) => injected(call, fragment, depth)
      case _ => placed(call, tag, fragment, depth).toVector
    }

  /** Whether a fragment reaching `depth` levels fits under the limit; reported at the call when it does not. */
  private def fits(call: AST.Call, depth: Int): Boolean =
    depth <= limit || {
      report(DepthExceeded(call.location, limit, fatal = false))
      false
    }

  /** `build`, or none when what it builds goes past the depth limit, which is reported at the call, or past the node
    * limit, the load's error at the first call to go past it.
    */
  private def fitting[A](call: AST.Call)(build: => A): Option[A] =
    try Some(build)
    catch {
      case TooDeep =>
        report(DepthExceeded(call.location, limit, fatal = false))
        None
      // the load's error, not the fragment's, so that a handler that drops the fragment does not drop it
      case TooLarge =>
        if (nodes.error.isEmpty) nodes.error = Some(FragmentTooLarge(call.location, call.name, nodes.limit))
        None
    }

  /** The fields of `map` standing `depth` levels deep. */
  private def statements(call: AST.Call, map: Node.IMap[?], depth: Int): Vector[AST.Statement] =
    map.iterator.map { child =>
      val value = built(call, child, None, depth)
      AST.Field(::(AST.Key(child.id.key, call.location), Nil), AST.Operator.Merge, value, None)
    }.toVector

  /** `node` as a value of the syntax tree with `depth` levels around it, its location kept as an origin behind the
    * call; a container past the limit throws [[TooDeep]].
    */
  private def built(call: AST.Call, node: Node.ISome[Id], tag: Option[String], depth: Int): AST.Value = {
    // counted before it is built, so that an alias in YAML used over and over cannot take the heap (§11)
    nodes.count()
    val location = DirectiveLocation(call.location, call.name, node.location)
    node match {
      case Node.IScalar(_, own, value, _) => AST.Scalar(value, tag.orElse(own), location)
      case Node.INull(_, own, _) => AST.Null(tag.orElse(own), location)
      case map: Node.IMap[?] =>
        if (depth >= limit) throw TooDeep
        AST.Block(statements(call, map, depth + 1), tag.orElse(map.tag), location)
      case seq: Node.ISeq[?] =>
        if (depth >= limit) throw TooDeep
        val elements = seq.iterator.map(e => AST.Element(built(call, e, None, depth + 1), None)).toVector
        AST.Sequence(elements, tag.orElse(seq.tag), location)
    }
  }

  /** The fragment a call gives, [[Fragment.None]] when it gives nothing or fails; a source it parses stands `base`
    * levels deep.
    */
  private def run(call: AST.Call, stack: List[Source], base: Int): Fragment =
    if (disabled(call.name)) {
      report(DisabledDirective(call.location, call.name))
      Fragment.None
    } else
      directives.get(call.name) match {
        case Some(directive) =>
          arguments(call, directive.parameters).fold[Fragment](Fragment.None) { arguments =>
            directive(arguments, new Context(call, stack, base)) match {
              case Left(error) =>
                report(DirectiveFailed(call.location, call.name, error))
                Fragment.None
              case Right(fragment) => fragment
            }
          }
        case None =>
          report(UnknownDirective(call.location, call.name))
          Fragment.None
      }

  /** The arguments of `call` by parameter — positional ones in order, then named ones — or none when one goes to no
    * parameter, or to one already given, or a required one is missing; every such argument reported.
    */
  private def arguments(call: AST.Call, parameters: Seq[Parameter]): Option[Node.IMap[Id.Root]] = {
    val names = parameters.map(_.name).toVector
    val bound = mutable.LinkedHashMap.empty[String, AST.Argument]
    val wrong = call.arguments.zipWithIndex.map {
      case (argument @ AST.Argument(None, _, at, _), index) =>
        if (index < names.length) bind(bound, names(index), argument)
        else fail(ExtraArgument(at))
      case (argument @ AST.Argument(Some(name), _, at, _), _) =>
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
        key -> (Tree.Scalar(bound(key).value, None, bound(key).valueLocation): Tree.Value))
      Some(MapImpl(Id.Root, Tree.Map(keys, immutable.Map(entries*), None, call.location)))
    }
  }

  private def bind(bound: mutable.Map[String, AST.Argument], name: String, argument: AST.Argument): Boolean = {
    bound(name) = argument
    true
  }

  private def fail(error: PsiError): Boolean = {
    report(error)
    false
  }

  private final class Context(call: AST.Call, stack: List[Source], base: Int) extends DirectiveContext {
    def location: SourceLocation = call.location

    def source: String = stack.head.name

    def url: Option[URL] = stack.head.url

    def parse(url: URL): Either[IOException, Fragment] =
      Source.read(url.toString, url, budget) match {
        case Right(Some(source)) => Right(nested(source))
        // the load's budget gone, which the load reports: nothing to give, and no error of the fragment's
        case Right(None) =>
          Right(Fragment.Parsed(AST.Document(Vector.empty), SourceLocation(url.toString, 1, 1), Nil, Nil))
        case Left(SourceUnreadable(_, cause)) => Left(cause)
        case Left(error) =>
          Right(Fragment.Parsed(AST.Document(Vector.empty), SourceLocation(url.toString, 1, 1), error :: Nil, Nil))
      }

    def parse(name: String, text: String): Fragment = nested(Source(name, text))

    def find[A](arguments: Node.IMap[Id.Root], extensions: Seq[String])(
        open: URL => Either[IOException, A]): Either[DirectiveError, Option[A]] =
      Include.find(arguments, extensions, url)(open)

    def read(url: URL): Either[IOException, Option[Array[Byte]]] = Source.bytes(url.toString, url, budget)

    private def nested(source: Source): Fragment = {
      val root = SourceLocation(source.name, 1, 1)
      if (stack.exists(_.key == source.key)) {
        val chain = (source :: stack).reverse.map(_.name)
        Fragment.Parsed(
          AST.Document(Vector.empty), root, DirectiveCycle(call.location, ::(chain.head, chain.tail)) :: Nil, Nil)
      } else if (stack.length >= MaxDepth)
        Fragment.Parsed(AST.Document(Vector.empty), root, DirectiveTooDeep(call.location, MaxDepth) :: Nil, Nil)
      else {
        val expand = new Expand(directives, disabled, markers, formatters, limit, budget, nodes)
        val document = expand.document(source, stack, base)
        nodes.made(Fragment.Parsed(document, root, expand.errors.toList, source.name :: expand.included.toList))
      }
    }
  }
}

private[psi] object Expand {

  /** How many sources deep directives may parse, a backstop for a key that changes on every step (§6). */
  def MaxDepth: Int = 64

  /** A node a handler built that goes past the depth limit. */
  private case object TooDeep extends ControlThrowable

  /** A node a handler built that goes past the node limit. */
  private case object TooLarge extends ControlThrowable

  /** The nodes fragments bring into the syntax trees of one load beyond its sources' own, at most `limit`, the load's
    * node limit, which grafts share once the sources are expanded (§11): every node a handler built from a `Node`,
    * every use counted, and every node of a source parsed but on its first use in the load that parsed it — a fragment
    * a handler keeps and gives again, in this load or a later one, copies what was read once.
    */
  final class Nodes(val limit: Int) {
    var used = 0L

    /** The error of the first call to go past the limit: the load's, as the read limit's is, so that a handler that
      * drops the fragment holding it does not drop it.
      */
    var error: Option[PsiError] = None

    /** The fragments this load parsed and has not used yet, by identity. */
    private val fresh =
      java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Fragment.Parsed, java.lang.Boolean])

    /** `fragment`, parsed in this load, its first use free. */
    def made(fragment: Fragment.Parsed): Fragment.Parsed = {
      val _ = fresh.add(fragment)
      fragment
    }

    /** Counts a use of `fragment`: none for the first in the load that parsed it, its nodes for any other. */
    def use(fragment: Fragment.Parsed): Unit = if (!fresh.remove(fragment)) count(size(fragment.document.statements))

    /** Counts `n` nodes more, before they are built; past the limit, [[TooLarge]]. */
    def count(n: Long = 1): Unit = {
      if (used + n > limit) throw TooLarge
      used += n
    }
  }

  /** How many nodes `statements` hold, a dotted key's keys after the first each a map. */
  private def size(statements: Vector[AST.Statement]): Long =
    statements.iterator.map {
      case field: AST.Field => field.keys.size - 1 + size(field.value)
      case _ => 1L
    }.sum

  /** How many nodes `value` holds, itself among them. */
  private def size(value: AST.Value): Long =
    value match {
      case AST.Block(statements, _, _) => 1 + size(statements)
      case AST.Sequence(elements, _, _) => 1 + elements.iterator.map(element => size(element.value)).sum
      case AST.Injections(items, _, _) => 1 + items.iterator.collect { case AST.Injected.Given(_, v) => size(v) }.sum
      // a scalar, a `null` and the rest, a marked block among them, being a scalar
      case _ => 1
    }

  /** How many levels `statements` open below where they stand, a dotted key's keys after the first counted. */
  private def levels(statements: Vector[AST.Statement]): Int =
    // an expanded document holds no directive
    statements.iterator.collect { case field: AST.Field => field.keys.size - 1 + height(field.value) }.foldLeft(0)(
      math.max)

  /** How many levels `value` opens. */
  private def height(value: AST.Value): Int =
    value match {
      case AST.Block(statements, _, _) => 1 + levels(statements)
      case AST.Sequence(elements, _, _) => 1 + elements.iterator.map(e => height(e.value)).foldLeft(0)(math.max)
      // what a call gave, a map or a sequence, stands where the block does
      case AST.Injections(items, _, _) =>
        items.iterator.collect { case AST.Injected.Given(_, value) => height(value) }.foldLeft(1)(math.max)
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
      formatters: Map[String, Formatter],
      limit: Int,
      budget: ReadBudget,
      nodes: Nodes): Result[(AST.Document, List[String])] = {
    val expand = new Expand(directives, disabled, markers, formatters, limit, budget, nodes)
    val document = expand.document(source, Nil, 0)
    Result((document, expand.included.toList), expand.errors.toList)
  }
}
