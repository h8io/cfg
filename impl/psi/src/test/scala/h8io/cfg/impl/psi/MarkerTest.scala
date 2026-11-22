package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.Registering.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkerTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def marker(markerName: String, params: Parameter*)(
      body: (Node.IMap[Id.Root], String) => Either[MarkerError, String]): (String, Marker) =
    markerName -> new Marker {
      def parameters: Seq[Parameter] = params
      def apply(arguments: Node.IMap[Id.Root], value: String): Either[MarkerError, String] = body(arguments, value)
    }

  private val upper: (String, Marker) = marker("upper")((_, value) => Right(value.toUpperCase))

  private val reverse: (String, Marker) = marker("~~")((_, value) => Right(value.reverse))

  private def loader(markers: (String, Marker)*): PsiLoader =
    Psi.builder.withMarkers(markers*).withSystemProperties(Map.empty).build.fold(e => fail(e.toString), identity)

  private def loaded(loader: PsiLoader, text: String): Node.IMap[Id.Root] =
    loader.parse("s", text).fold(errors => fail(errors.toString), identity)

  private def errors(loader: PsiLoader, text: String): List[PsiError] =
    loader.parse("s", text).fold(_.errors, root => fail(root.toString))

  private def value(node: h8io.cfg.INode[Id]): String =
    node match {
      case Node.IScalar(_, _, value, _) => value
      case other => fail(other.toString)
    }

  "A builder" should "check the markers' names and parameters" in {
    val twice = marker("twice", Parameter("p", required = true), Parameter("p", required = false))((_, v) => Right(v))
    Psi.builder.withMarkers(upper, marker("<")((_, v) => Right(v)), marker("a+")((_, v) => Right(v))).withMarkers(
      marker("")((_, v) => Right(v)),
      twice,
      upper,
      reverse,
      marker("_x-1")((_, v) => Right(v))).build shouldBe
      Left(
        PsiBuildErrors(
          ::(
            DuplicateMarker("<"),
            List(
              InvalidMarkerName("a+"),
              InvalidMarkerName(""),
              DuplicateMarkerParameter("twice", "p"),
              DuplicateMarker("upper")))))
  }

  "The loader's markers" should "run left to right over the block's value, after the built-in ones" in {
    val root =
      loaded(loader(upper, reverse), "a: \"\"\"< upper ~~\n    abc\n  d\n  \"\"\"\nb: '''upper\n  x\\ty\n  '''")
    root("a") shouldBe Node.IScalar(Id.Key("a", Id.Root), None, "D\nCBA", at(1, 4))
    value(root("b")) shouldBe "X\\TY"
  }

  they should "run over a block indented below its key, which takes the tag after the key" in {
    loaded(loader(upper), "u: @s\n  \"\"\"upper\n  abc\n  \"\"\"")("u") shouldBe
      Node.IScalar(Id.Key("u", Id.Root), Some("s"), "ABC", at(2, 3))
  }

  they should "get the value with its escapes read and its substitutions resolved" in {
    val root = loaded(loader(upper), "h: db\nu: @t \"\"\"upper\n  jdbc:${h}\\t\n  \"\"\"\nv: ${u}")
    root("u") shouldBe
      Node.IScalar(
        Id.Key("u", Id.Root),
        Some("t"),
        "JDBC:DB\t",
        ConcatenationLocation(at(2, 7), ::(ReferenceLocation(at(3, 8), "h", at(1, 4)), Nil)))
    value(root("v")) shouldBe "JDBC:DB\t"
  }

  they should "get their arguments by parameter, every literal a string" in {
    var received: List[Node.ISome[Id.Key]] = Nil
    val replace = marker("replace", Parameter("from", required = true), Parameter("to", required = false)) {
      (arguments, value) =>
        received = arguments.toList
        Right(value)
    }
    loaded(loader(replace), "a: \"\"\"replace(1, to: x)\n  v\n  \"\"\"").size shouldBe 1
    received shouldBe
      List(
        Node.IScalar(Id.Key("from", Id.Root), None, "1", at(1, 15)),
        Node.IScalar(Id.Key("to", Id.Root), None, "x", at(1, 22)))
    loaded(loader(replace), "a: \"\"\"replace('y')\n  v\n  \"\"\"").size shouldBe 1
    received.map(value) shouldBe List("y")
  }

  they should "not run with an argument wrong, the block read as with no marker" in {
    var runs = 0
    val counted = marker("counted", Parameter("p", required = true)) { (_, value) =>
      runs += 1
      Right(value)
    }
    errors(loader(counted), "a: \"\"\"counted\n  v\n  \"\"\"\nb: \"\"\"counted(1, 2)\n  v\n  \"\"\"") shouldBe
      List(MissingArgument(at(1, 7), "p"), ExtraArgument(at(4, 18)))
    runs shouldBe 0
  }

  they should "get an integer argument as written, -0 included" in {
    val echo = marker("echo", Parameter("n", required = true))((arguments, _) => Right(value(arguments("n"))))
    val root = loaded(loader(echo), "a: \"\"\"echo(-0)\n  x\n  \"\"\"\nb: '''echo(n: 12)\n  x\n  '''")
    value(root("a")) shouldBe "-0"
    value(root("b")) shouldBe "12"
  }

  "A built-in marker" should "not follow one of the loader's" in {
    errors(loader(upper), "a: \"\"\"upper <\n  v\n  \"\"\"") shouldBe List(BuiltInMarkerAfterLoaders(at(1, 13), "<"))
  }

  "A marker that fails" should "be reported at the marker, what depends on the block not failing with it" in {
    val failing = marker("fails")((_, _) => Left(Problem("no")))
    errors(loader(upper, failing), "a: \"\"\"upper fails\n  v\n  \"\"\"\nb: ${a}") shouldBe
      List(MarkerFailed(at(1, 13), "fails", Problem("no")))
  }

  "A marked block" should "be caught in a cycle, and resolved once however many refer to it" in {
    val identity = marker("identity")((_, value) => Right(value))
    errors(loader(identity), "a: \"\"\"identity\n  ${a}\n  \"\"\"") shouldBe List(SubstitutionCycle(at(2, 3), "a"))
    errors(loader(identity), "a: \"\"\"identity\n  ${b}\n  \"\"\"\nb: \"\"\"identity\n  ${a}\n  \"\"\"") shouldBe
      List(SubstitutionCycle(at(5, 3), "a"))
    errors(loader(identity), "a: \"\"\"identity\n  ${missing}\n  \"\"\"\nb: ${a}\nc: ${a}") shouldBe
      List(UnresolvedSubstitution(at(2, 3), "missing"))
  }

  it should "be a scalar with nothing to look into" in {
    errors(loader(upper), "a: \"\"\"upper\n  v\n  \"\"\"\nb: ${a.x}") shouldBe
      List(UnresolvedSubstitution(at(4, 4), "a.x"))
  }

  it should "be resolved before it is wrapped, grafted or shown, which would be a bug of the loader" in {
    val marked = Tree.Marked(Tree.Scalar("v", None, at(1, 4)), Vector.empty)
    the[IllegalStateException] thrownBy wrap(Id.Root, marked) should have message "An unresolved substitution at s:1:4"
    the[IllegalStateException] thrownBy Resolve.graft(marked, at(2, 4), Id.Key("b", Id.Root), None) should
      have message "An unresolved substitution at s:1:4 grafted"
    the[IllegalStateException] thrownBy Render.value(marked) should have message "Unresolved markers at s:1:4"
    val written = AST.Marked(AST.Scalar("v", Some("t"), at(1, 4)), Vector.empty)
    (written.tag, written.location) shouldBe ((Some("t"), at(1, 4)))
    val onNull = Tree.Marked(Tree.Null(None, at(1, 4)), Vector.empty)
    the[IllegalStateException] thrownBy
      Resolve(Tree.Map(Vector("a"), Map("a" -> onNull), None, at(1, 1))) should have message
      "Markers on a non-scalar at s:1:4"
  }

  "A block with markers" should "run them below an optional substitution that finds nothing, the block standing" in {
    value(loaded(loader(upper), "h: db\nu: \"\"\"upper\n  jdbc:${h}\n  \"\"\"\nu: $?missing")("u")) shouldBe "JDBC:DB"
    value(loaded(loader(upper), "u: \"\"\"upper\n  abc\n  \"\"\"\nu: $?missing")("u")) shouldBe "ABC"
  }
}
