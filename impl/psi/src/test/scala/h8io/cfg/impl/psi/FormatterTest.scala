package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.Registering.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FormatterTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def formatter(params: Parameter*)(
      body: Node.IMap[Id.Root] => Either[FormatterError, Node.Some => Either[FormatterError, String]]): Formatter =
    new Formatter {
      def parameters: Seq[Parameter] = params
      def apply(arguments: Node.IMap[Id.Root]): Either[FormatterError, Node.Some => Either[FormatterError, String]] =
        body(arguments)
    }

  /** Gives what it formats, a container as its `toString` shows it, its id in front. */
  private val show: Formatter =
    formatter() { _ =>
      Right { node =>
        val shown = node match {
          case Node.IScalar(_, _, value, _) => value
          case _: Node.INull[?] => "null"
          case other => other.toString
        }
        Right(node.id.toString + "=" + shown)
      }
    }

  private val upper: Formatter =
    formatter() { _ =>
      Right {
        case Node.IScalar(_, _, value, _) => Right(value.toUpperCase)
        case _ => Left(Problem("no scalar"))
      }
    }

  private def loader(formatters: (String, Formatter)*): PsiLoader =
    Psi.builder.withFormatters(formatters*).withSystemProperties(Map.empty).build.fold(e => fail(e.toString), identity)

  private val plain: PsiLoader = loader()

  private def loaded(text: String, loader: PsiLoader = plain): Node.IMap[Id.Root] =
    loader.parse("s", text).fold(errors => fail(errors.toString), identity)

  private def errors(text: String, loader: PsiLoader = plain): List[PsiError] =
    loader.parse("s", text).fold(_.errors, root => fail(root.toString))

  private def value(node: h8io.cfg.INode[Id]): String =
    node match {
      case Node.IScalar(_, _, value, _) => value
      case other => fail(other.toString)
    }

  "join" should "put the elements of a sequence one after another with the separator between them" in {
    val root =
      loaded("h: [a, b, c]\nurl: \"jdbc:${h; join(\",\")}/db\"\nall: ${h; join(\" | \")}\none: ${?h; join('')}")
    value(root("url")) shouldBe "jdbc:a,b,c/db"
    value(root("all")) shouldBe "a | b | c"
    value(root("one")) shouldBe "abc"
  }

  it should "give nothing for an empty sequence, and take its separator by name, as an integer or a bare name" in {
    val root = loaded("e: []\nh: [a, b]\nx: \"<${e; join(\",\")}>\"\ny: ${h; join(separator: 0)}\nz: ${h; join(or)}")
    value(root("x")) shouldBe "<>"
    value(root("y")) shouldBe "a0b"
    value(root("z")) shouldBe "aorb"
  }

  it should "be a scalar located as a concatenation, with only the tag written before it" in {
    loaded("h: @s [a, b]\nu: @t ${h; join(\",\")}\nv: ${h; join(\",\")}") shouldBe
      loaded("h: @s [a, b]\nu: @t ${h; join(\",\")}\nv: ${h; join(\",\")}")
    val root = loaded("h: @s [a, b]\nu: @t ${h; join(\",\")}\nv: ${h; join(\",\")}")
    root("u") shouldBe
      Node.IScalar(
        Id.Key("u", Id.Root),
        Some("t"),
        "a,b",
        ConcatenationLocation(at(2, 7), ::(ReferenceLocation(at(2, 7), "h", at(1, 7)), Nil)))
    root("v") match {
      case Node.IScalar(_, tag, "a,b", _) => tag shouldBe None
      case other => fail(other.toString)
    }
  }

  it should "stand as one element in brackets and after a '- ', and take a tag from a block above it" in {
    val root = loaded("h: [a, b]\ns: [x, ${h; join(\",\")}]\nt:\n  - ${h; join(\"-\")}\nu: @k\n  ${h; join(\"+\")}")
    def elements(node: h8io.cfg.INode[Id]): List[String] =
      node match {
        case seq: Node.ISeq[?] => seq.iterator.map(value).toList
        case other => fail(other.toString)
      }
    elements(root("s")) shouldBe List("x", "a,b")
    elements(root("t")) shouldBe List("a-b")
    root("u") match {
      case Node.IScalar(_, Some("k"), "a+b", _) => succeed
      case other => fail(other.toString)
    }
  }

  "A formatter in quotes" should "be read by its own rules, a quote or a brace in an argument closing nothing" in {
    val root = loaded(
      "h: [a, b]\nq: \"[${h; join(\", \")}]\"\nb: \"${h; join(\"}\")}\"\nt: \"\"\"\n  {${h; join(\"\\\"\")}}\n  \"\"\"")
    value(root("q")) shouldBe "[a, b]"
    value(root("b")) shouldBe "a}b"
    value(root("t")) shouldBe "{a\"b}"
  }

  it should "be text in a raw string, and after a substitution with no braces" in {
    val root = loaded("h: x\nr: '${h; join(\",\")}'\nb: $h;join")
    value(root("r")) shouldBe "${h; join(\",\")}"
    value(root("b")) shouldBe "x;join"
  }

  "An optional substitution that finds nothing" should "call no formatter, its field or element left out" in {
    var calls = 0
    val counted = formatter() { _ =>
      Right { _ =>
        calls += 1
        Right("x")
      }
    }
    val root =
      loaded("a: ${?nope; counted}\nb: \"<${?nope; counted}>\"\nc: [${?nope; counted}]", loader("counted" -> counted))
    root.iterator.map(_.id.key).toList shouldBe List("b", "c")
    value(root("b")) shouldBe "<>"
    root("c") match {
      case seq: Node.ISeq[?] => seq.size shouldBe 0
      case other => fail(other.toString)
    }
    calls shouldBe 0
  }

  "A loader's formatter" should "get what the path finds, resolved, at the position it is defined at" in {
    val root = loaded(
      "m { x: 1, y: [p, ${n}] }\nn: null\ns: \"v\"\na: ${m; show}\nb: ${n; show}\nc: \"${m.y; show}\"\nd: ${s; show}",
      loader("show" -> show))
    value(root("a")) shouldBe "Key(m,Root)=x: 1\ny:\n  - p\n  - null"
    value(root("b")) shouldBe "Key(n,Root)=null"
    value(root("c")) shouldBe "Key(y,Key(m,Root))=- p\n- null"
    value(root("d")) shouldBe "Key(s,Root)=v"
  }

  it should "get its arguments by parameter, called once for them and once for each node" in {
    var bound: List[List[String]] = Nil
    var formatted = 0
    val wrap = formatter(Parameter("left", required = true), Parameter("right", required = false)) { arguments =>
      val sides = arguments.toList.map(value)
      bound = sides :: bound
      Right { node =>
        formatted += 1
        Right(sides.head + value(node) + sides.lastOption.filter(_ => sides.size > 1).getOrElse(""))
      }
    }
    val root = loaded(
      "a: x\nb: ${a; wrap(\"<\", right: '>')}\nc: \"${a; wrap(1)}${a; wrap(left: '[')}\"", loader("wrap" -> wrap))
    value(root("b")) shouldBe "<x>"
    value(root("c")) shouldBe "1x[x"
    bound.reverse shouldBe List(List("<", ">"), List("1"), List("["))
    formatted shouldBe 3
  }

  it should "be called by a name in backticks, and with no parentheses" in {
    val root = loaded("a: x\nb: ${a; `to upper`}\nc: ${a; upper}", loader("to upper" -> upper, "upper" -> upper))
    value(root("b")) shouldBe "X"
    value(root("c")) shouldBe "X"
  }

  it should "be run in a block with the loader's markers" in {
    val identity = new Marker {
      def parameters: Seq[Parameter] = Nil
      def apply(arguments: Node.IMap[Id.Root], value: String): Either[MarkerError, String] = Right(value + "!")
    }
    val by =
      Psi.builder.withMarker("bang", identity).withFormatter("upper", upper).build.fold(e => fail(e.toString), x => x)
    value(loaded("a: x\nb: \"\"\"bang\n  ${a; upper}\n  \"\"\"", by)("b")) shouldBe "X!"
  }

  "A formatted value" should "be replaced by a map merged over it, and replace one" in {
    val root = loaded("h: [a]\nx: ${h; join(\",\")}\nx.p: 1\ny { q: 2 }\ny: ${h; join(\",\")}")
    root("x") match {
      case map: Node.IMap[?] => map.iterator.map(node => node.id.key -> value(node)).toList shouldBe List("p" -> "1")
      case other => fail(other.toString)
    }
    value(root("y")) shouldBe "a"
  }

  it should "count as the element it gives in a sequence, none when an optional one finds nothing" in {
    val root = loaded("h: [a]\ns: [${?missing; join(',')}, first, ${h; join(',')}, last]\nx: $s[0]\ny: $s[2]")
    value(root("x")) shouldBe "first"
    value(root("y")) shouldBe "last"
  }

  it should "leave what it is merged over when an optional one finds nothing, and replace it otherwise" in {
    val root = loaded("h: [b]\na { x: 1 }\na: ${?missing; join(',')}\nb { x: 1 }\nb: ${h; join(',')}\nc: 1\nc: $?gone")
    root("a").toString shouldBe "x: 1"
    value(root("b")) shouldBe "b"
    errors("a { x: 1 }\na: ${x; join(',')}\nx: 1") shouldBe
      List(FormatterFailed(at(2, 9), "join", JoinNotSequence(at(3, 4))))
  }

  it should "be a scalar to look into, and a cycle through itself" in {
    errors("h: [a]\nx: ${h; join(\",\")}\ny: ${x.p}") shouldBe List(UnresolvedSubstitution(at(3, 4), "x.p"))
    errors("x: ${x; join(\",\")}") shouldBe List(SubstitutionCycle(at(1, 4), "x"))
    errors("x: \"${x; join(\",\")}\"") shouldBe List(SubstitutionCycle(at(1, 5), "x"))
    value(loaded("h: [a, b]\nx: ${h; join(\",\")}\ny: ${x}")("y")) shouldBe "a,b"
  }

  "join's errors" should "be what it joins being no sequence, or an element no scalar, null included" in {
    errors("h: a\nx: ${h; join(\",\")}") shouldBe
      List(FormatterFailed(at(2, 9), "join", JoinNotSequence(at(1, 4))))
    errors("h: [a, null]\nx: \"${h; join(\",\")}\"") shouldBe
      List(FormatterFailed(at(2, 10), "join", JoinElementNotScalar(1, at(1, 8))))
    errors("h: [[a]]\nx: ${h; join(\",\")}") shouldBe
      List(FormatterFailed(at(2, 9), "join", JoinElementNotScalar(0, at(1, 5))))
  }

  "A loader's formatter that fails" should "be reported at its name, for its arguments with the syntax's errors" in {
    val refusing = formatter()(_ => Left(Problem("no arguments")))
    errors("a: ${x; refusing}\nb: [", loader("refusing" -> refusing)) shouldBe
      List(FormatterFailed(at(1, 9), "refusing", Problem("no arguments")), UnclosedBracket(at(2, 4), '['))
    errors("a: [x]\nb: ${a; upper}\nc: ${b}", loader("upper" -> upper)) shouldBe
      List(FormatterFailed(at(2, 9), "upper", Problem("no scalar")))
  }

  "A formatter's arguments" should "go to its parameters, the handler not called otherwise" in {
    var calls = 0
    val counted = formatter(Parameter("p", required = true)) { _ =>
      calls += 1
      Right(_ => Right(""))
    }
    errors("a: ${x; counted}\nb: ${x; counted(1, 2)}\nc: ${x; counted(q: 1)}", loader("counted" -> counted)) shouldBe
      List(
        MissingArgument(at(1, 9), "p"),
        ExtraArgument(at(2, 20)),
        MissingArgument(at(3, 9), "p"),
        UnknownArgument(at(3, 17), "q"))
    calls shouldBe 0
    errors("a: ${x; join}\nb: ${x; join(\",\", 2)}\nc: ${x; join(p: 1, \",\")}") shouldBe
      List(
        MissingArgument(at(1, 9), "separator"),
        ExtraArgument(at(2, 19)),
        MissingArgument(at(3, 9), "separator"),
        UnknownArgument(at(3, 14), "p"),
        PositionalAfterNamed(at(3, 20))
      )
  }

  "An unknown formatter" should "be reported at its name" in {
    errors("a: ${x; nope(1)}") shouldBe List(UnknownFormatter(at(1, 9), "nope"))
  }

  "A formatter that cannot be read" should "be reported where it stops fitting, the substitution skipped" in {
    errors("a: ${x;}") shouldBe List(InvalidFormatter(at(1, 8)))
    errors("a: ${x; }") shouldBe List(InvalidFormatter(at(1, 9)))
    errors("a: ${x; 1}") shouldBe List(InvalidFormatter(at(1, 9)))
    errors("a: ${x; join(\",\")x}") shouldBe List(InvalidFormatter(at(1, 18)))
    errors("a: ${x; join(\",\"\nb: 1") shouldBe List(InvalidFormatter(at(1, 17)))
    errors("a: \"${x; join(\"\"\nb: 1") shouldBe List(UnterminatedString(at(1, 4), "\""), InvalidFormatter(at(1, 17)))
    errors("a: ${x; join(\"," + 0x202e.toChar.toString + "\")}") shouldBe
      // what follows the character is read anew, as after a broken substitution
      List(
        InvalidFormatter(at(1, 14)),
        InvisibleCharacter(at(1, 16), 0x202e.toChar),
        UnterminatedString(at(1, 17), "\""))
    errors("a: ${x; `join}") shouldBe List(InvalidFormatter(at(1, 9)))
  }

  it should "be one, with no whitespace before the '}' or the ';'" in {
    errors("a: ${x; join(\",\"); join(\",\")}") shouldBe List(SecondFormatter(at(1, 18)))
    errors("a: ${x; join(\",\") }") shouldBe List(WhitespaceInSubstitution(at(1, 18)))
    errors("a: ${x ; join(\",\")}") shouldBe List(WhitespaceInSubstitution(at(1, 7)))
  }

  it should "be no substitution in a directive's argument" in {
    errors("#include(file: \"${x; join(\",\")}\")*") shouldBe List(SubstitutionInArgument(at(1, 17)))
  }

  "A formatted substitution" should "inject nothing: marked, an error, and alone among fields a value with no key" in {
    errors("h: [a]\nb: ${h*; join(\",\")}") shouldBe List(FormattedInjection(at(2, 4)))
    errors("h: [a]\nb: [${h*; join(\",\")}]") shouldBe List(FormattedInjection(at(2, 5)))
    errors("h: [a]\nb { ${h; join(\",\")} }") shouldBe List(ValueAmongFields(at(2, 5)))
    errors("h: [a]\nb:\n  - x\n  ${h; join(\",\")}") shouldBe List(UndashedElement(at(4, 3)))
    errors("h: [a]\nb:\n  $h*\n  ${h; join(\",\")}") shouldBe List(ValueAmongFields(at(4, 3)))
  }

  it should "be written back as it is written" in {
    val formatted = Token.Formatted(
      Token.Reference(Id.Key("h", Id.Root), optional = true, at(1, 4)),
      Token.Join(", ", "join(\", \")", at(1, 10)))
    Token.written(Vector(Token.Text("<"), formatted)) shouldBe "<${?h; join(\", \")}"
    errors("\"${h; join(\",\")}\": 1") shouldBe List(QuotedKey(at(1, 1), "${h; join(\",\")}"))
  }

  it should "be resolved before it is wrapped or grafted, which would be a bug of the loader" in {
    val formatted = Tree.Formatted(
      Token.Formatted(
        Token.Reference(Id.Key("h", Id.Root), optional = false, at(1, 4)), Token.Join(",", "join(\",\")", at(1, 9))),
      None,
      at(1, 4))
    the[IllegalStateException] thrownBy wrap(Id.Root, formatted) should have message
      "An unresolved substitution at s:1:4"
    the[IllegalStateException] thrownBy Resolve.graft(formatted, at(2, 4), Id.Key("b", Id.Root), None) should
      have message "An unresolved substitution at s:1:4 grafted"
  }

  "What join builds" should "count against the size limit, what a loader's formatter gives alone not" in {
    def limited(characters: Int, formatters: (String, Formatter)*): PsiLoader =
      Psi.builder.withFormatters(formatters*).withSizeLimit(100, characters).build.fold(e => fail(e.toString), x => x)
    val long = formatter()(_ => Right(_ => Right("x" * 10)))
    errors("h: [ab, cd]\nx: ${h; join(\",\")}", limited(4)) shouldBe List(ConcatenationTooLong(at(2, 4), 4))
    loaded("h: [ab, cd]\nx: ${h; join(\",\")}", limited(5)).size shouldBe 2
    loaded("x: ${x0; long}\nx0: a", limited(0, "long" -> long)).size shouldBe 2
    errors("x: \"${x0; long}\"\nx0: a", limited(9, "long" -> long)) shouldBe List(ConcatenationTooLong(at(1, 4), 9))
  }

  "A builder" should "check the formatters' names and parameters" in {
    val twice = formatter(Parameter("p", required = true), Parameter("p", required = false))(_ => Right(_ => Right("")))
    Psi.builder.withFormatters("join" -> upper, "twice" -> twice, "upper" -> upper, "upper" -> show).build shouldBe
      Left(
        PsiBuildErrors(
          ::(DuplicateFormatter("join"), List(DuplicateFormatterParameter("twice", "p"), DuplicateFormatter("upper")))))
  }
}
