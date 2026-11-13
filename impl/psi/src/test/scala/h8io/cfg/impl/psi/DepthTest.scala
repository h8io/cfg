package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DepthTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int, source: String = "s"): SourceLocation = SourceLocation(source, line, column)

  private def beyond(line: Int, column: Int, source: String = "s"): DepthExceeded =
    DepthExceeded(at(line, column, source), 3, fatal = true)

  private def directive(directiveName: String)(body: DirectiveContext => Option[Fragment]): Directive =
    new Directive {
      def name: String = directiveName
      def parameters: Seq[Parameter] = Nil
      def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Option[Fragment]] =
        Right(body(context))
    }

  private def loader(limit: Int, directives: Directive*): PsiLoader =
    Psi.builder
      .withDirectives(directives*)
      .withSystemProperties(Map.empty)
      .withDepthLimit(limit)
      .build
      .fold(e => fail(e.toString), identity)

  private def errors(text: String, directives: Directive*): List[PsiError] =
    loader(3, directives*).parse("s", text).fold(_.errors, root => fail(root.toString))

  private def loads(text: String, directives: Directive*): Boolean = loader(3, directives*).parse("s", text).isRight

  private def parsed(text: String): Result[Ast.Document] = Parser.parse(Source("s", text), Map.empty, 0, 3)

  "A depth limit" should "be at least 1" in {
    Psi.builder.withDepthLimit(0).build shouldBe Left(PsiBuildErrors(::(InvalidDepthLimit(0), Nil)))
    Psi.builder.withDepthLimit(1).build.isRight shouldBe true
  }

  "A source's text" should "open as many levels as the limit, and end where it opens one more" in {
    loads("a { b { c { } } }") shouldBe true
    errors("a { b { c { d { } } } }") shouldBe List(beyond(1, 15))
    loads("a: [[[]]]") shouldBe true
    errors("a: [[[[]]]]") shouldBe List(beyond(1, 7))
    errors("a: [[[[") shouldBe List(beyond(1, 7))
  }

  it should "count each key after a dot as a level, and the parentheses of a directive's arguments" in {
    loads("a.b.c.d: 1") shouldBe true
    errors("a.b.c.d.e: 1") shouldBe List(beyond(1, 8))
    errors("a.b { c: [ [1] ] }") shouldBe List(beyond(1, 12))
    loads("a.b { c := [1] }\nd: 1") shouldBe true
    errors("a { b { c { d: #x(1) } } }") shouldBe List(beyond(1, 18))
  }

  it should "keep what was read before it ended" in {
    parsed("x: 1\na: [[[[]]]]\nb: 2") shouldBe
      Result(
        Ast.Document(Vector(Ast.Field(::(Ast.Key("x", at(1, 1)), Nil), Ast.Operator.Merge,
          Ast.Scalar("1", Set.empty, at(1, 4)), None))),
        List(beyond(2, 7)))
  }

  it should "end where skipping a broken item opens one more" in {
    errors("a: 1 [[[[]]]]") shouldBe List(MissingSeparator(at(1, 6), "'['"), beyond(1, 9))
    errors("a: 1 #x((((") shouldBe List(MissingSeparator(at(1, 6), "'#x'"), beyond(1, 11))
  }

  "A source a directive parses" should "be read from the depth of the call" in {
    val text = directive("t")(context => Some(context.parse("t", "c { d { } }")))
    errors("a { b { #t } }", text) shouldBe List(beyond(1, 7, "t"))
    errors("a { b: #t }", text) shouldBe List(beyond(1, 7, "t"))
    loads("a: #t", text) shouldBe true
  }

  "A fragment" should "fail its call where it does not fit" in {
    var cached = Option.empty[Fragment]
    val cache = directive("cache") { context =>
      if (cached.isEmpty) cached = Some(context.parse("t", "c { }"))
      cached
    }
    errors("x: #cache\na { b { y: #cache } }", cache) shouldBe
      List(DepthExceeded(at(2, 12), 3, fatal = false))
    errors("a { b { c { #cache } } }", cache) shouldBe List(DepthExceeded(at(1, 13), 3, fatal = false))
  }

  it should "fail its call when built from a node too deep for where it lands" in {
    val node = Psi.parse("n", "p { q { r { } } }").fold(e => fail(e.toString), identity)
    val built = directive("built")(_ => Some(Fragment(node)))
    loads("#built", built) shouldBe true
    errors("v: #built", built) shouldBe List(DepthExceeded(at(1, 4), 3, fatal = false))
    val list = Psi.parse("n", "l: [[[]]]").fold(e => fail(e.toString), identity)("l") match {
      case seq: Node.ISeq[?] => seq
      case other => fail(other.toString)
    }
    val listed = directive("listed")(_ => Some(Fragment(list)))
    loads("v: #listed", listed) shouldBe true
    errors("a { v: #listed }", listed) shouldBe List(DepthExceeded(at(1, 8), 3, fatal = false))
  }

  "A substitution" should "fail where the value it grafts, resolved already, does not fit" in {
    // `a` is resolved first, from the root, and grafted again as it was resolved
    loads("b { c: 1 }\na: ${b}\nx { y: ${a} }") shouldBe true
    errors("b { c { } }\na: ${b}\nx { y { z: ${a} } }") shouldBe List(DepthExceeded(at(3, 12), 3, fatal = false))
  }

  "Resolution" should "fail, once, at the innermost substitution when it goes past the limit" in {
    loads("a: ${b}\nb: ${c}\nc: ${d}\nd: 1") shouldBe true
    errors("a: ${b}\nb: ${c}\nc: ${d}\nd: ${e}\ne: 1") shouldBe List(ResolutionTooDeep(at(4, 4), "e", 3))
  }

  it should "count the levels it descends with the substitutions it follows" in {
    loads("m { n { o: ${p} } }\np: 1") shouldBe true
    errors("m { n { o: ${p} } }\np { q: 1 }") shouldBe List(ResolutionTooDeep(at(1, 12), "p", 3))
    // a path is looked up with no step for its levels
    loads("m { n { o: \"${p.q}\" } }\np { q: 1 }") shouldBe true
    errors("m { n { o: \"${p}\" } }\np: ${r}\nr: 1") shouldBe List(ResolutionTooDeep(at(2, 4), "r", 3))
  }

  "Input of any depth" should "give errors, never overflow the stack, at the default limit" in {
    val n = 100000
    val inputs = List(
      "a: " + "[" * n + "]" * n,
      "a " + "{ a " * n + "{}" + " }" * n,
      "a: " + "[" * n,
      Seq.fill(n)("a").mkString(".") + ": 1",
      "#x(" + "(" * n,
      "a: 1 " + "[" * n,
      (1 to n).map(i => "a" + i.toString + ": $" + "{a" + (i + 1).toString + "}").mkString("\n")
    )
    inputs.map(text => Psi.parse("s", text).isLeft) shouldBe inputs.map(_ => true)
  }
}
