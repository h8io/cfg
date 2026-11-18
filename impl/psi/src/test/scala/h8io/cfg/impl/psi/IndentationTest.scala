package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The indented syntax (`format.md` §4, Indentation). */
class IndentationTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def loaded(text: String): Node.IMap[Id.Root] = Psi.parse("s", text).fold(e => fail(e.message), identity)

  /** The source as it loads, written back. */
  private def shown(text: String): String = loaded(text).toString

  private def errors(text: String): List[PsiError] = Psi.parse("s", text).fold(_.errors, root => fail(root.toString))

  "A block opened by indentation" should "hold the fields of a map, as braces do" in {
    shown("server:\n  host: example.com\n  ports:\n    - 80\n    - 443\n  tls { enabled: true, protocols: [TLSv1.3] }") shouldBe
      "server:\n  host: example.com\n  ports:\n    - 80\n    - 443\n  tls:\n    enabled: true\n    protocols:\n" +
      "      - TLSv1.3\n"
    shown("a:\n  b.c: 1\n  d := 2\n  e {}\n  f:\n      g: 3") shouldBe
      "a:\n  b:\n    c: 1\n  d: 2\n  e: {}\n  f:\n    g: 3\n"
    // any deeper indent opens a block, with no step fixed per file
    shown("a:\n b:\n          c: 1\n d: 2") shouldBe "a:\n  b:\n    c: 1\n  d: 2\n"
  }

  it should "merge as `{…}` after the same `:` merges, and replace after `:=`" in {
    shown("a { x: 1 }\na:\n  y: 2\nb { x: 1 }\nb :=\n  y: 2") shouldBe "a:\n  x: 1\n  y: 2\nb:\n  y: 2\n"
  }

  it should "hold one value, the key's, as on the line of the `:`" in {
    shown("a:\n  x\nb:\n  \"y z\"\nc:\n  [1, 2]\nd:\n  {}\ne:\n  null\nf:\n  '''\n  raw\n  '''") shouldBe
      "a: x\nb: \"y z\"\nc:\n  - 1\n  - 2\nd: {}\ne: null\nf: raw\n"
    shown("a: 1\nb:\n  $a\nc:\n  \"${a}2\"\nd:\n  #env(name: PSI_NO_SUCH_VARIABLE, default: v)") shouldBe
      "a: 1\nb: 1\nc: 12\nd: v\n"
  }

  it should "give the tags after its key to its value, added to the value's own" in {
    loaded("a: @s\n  @t x\nb: @m\n  k: v\nc: @q\n  - 1\nd: @n\n  null\ne: @e\n  {}")("a") shouldBe
      Node.IScalar(Id.Key("a", Id.Root), Set("s", "t"), "x", at(2, 6))
    shown("b: @m\n  k: v\nc: @q\n  - 1\nd: @n\n  null\ne: @e\n  {}\nf: @f\n  [1]\ng: @g\n  $b.k\nh: @h\n  \"${b.k}!\"") shouldBe
      "b: @m\n  k: v\nc: @q\n  - 1\nd: @n null\ne: @e {}\nf: @f\n  - 1\ng: @g v\nh: @h v!\n"
    shown("a: @t\n  '''\n  x\n  '''") shouldBe "a: @t x\n"
    loaded("x: 1\nc: @c\n  \"${x}y\"")("c") shouldBe Node.IScalar(Id.Key("c", Id.Root), Set("c"), "1y",
      ConcatenationLocation(at(3, 3), ::(ReferenceLocation(at(3, 4), "x", at(1, 4)), Nil)))
  }

  it should "inject a map's fields with a `*`, alone or among fields, and graft a node without one" in {
    shown("m { x: 1 }\na:\n  $m*\nb:\n  $m*\n  y: 2\nc:\n  y: 2\n  ${m*}") shouldBe
      "m:\n  x: 1\na:\n  x: 1\nb:\n  x: 1\n  y: 2\nc:\n  y: 2\n  x: 1\n"
    shown("m { x: 1 }\na: @t\n  $m*") shouldBe "m:\n  x: 1\na: @t\n  x: 1\n"
  }

  it should "take a directive alone as its value, and among fields as a statement" in {
    // `#env` gives a scalar, which in statement position is an error
    errors("a:\n  #env(name: PSI_NO_SUCH_VARIABLE, default: v)\n  b: 1") shouldBe List(DirectiveNotMap(at(2, 3), "env"))
    errors("a:\n  b: 1\n  #env(name: PSI_NO_SUCH_VARIABLE, default: v)") shouldBe
      List(DirectiveNotMap(at(3, 3), "env"))
  }

  it should "take a line that starts with a doc comment as starting where the comment does" in {
    shown("a:\n  x: 1\n/** d */ b: 2") shouldBe "a:\n  x: 1\nb: 2\n"
    errors("   /** d */ b: 2") shouldBe List(UnexpectedIndent(at(1, 4)))
    errors("a:\n    x: 1\n  /** d */ y: 2") shouldBe List(MisalignedIndent(at(3, 3)))
  }

  it should "count a line of tags with its value below as a level, so that no chain of them overflows the stack" in {
    val text = "a: @t\n" + (1 to 2000).map(i => " " * (i + 1) + "@t\n").mkString + " " * 2003 + "x"
    Psi.parse("s", text).fold(_.errors.map(_.getClass), root => fail(root.toString)) shouldBe
      List(classOf[DepthExceeded])
  }

  it should "take a doc comment before a field, on a line of its own, wherever it stands" in {
    loaded("/** d */\na:\n      /** e */\n  x: 1\n/** f */\nb: 3").size shouldBe 2
    errors("a:\n  /** e */\n  x") shouldBe List(MisplacedDocComment(at(2, 3)))
  }

  "`key:` at the end of a line" should "be an error with nothing deeper below it" in {
    errors("a:\nb: 1") shouldBe List(NothingIndented(at(1, 3)))
    errors("a:") shouldBe List(MissingValue(at(1, 3), "the end of the source"))
    errors("a:\n") shouldBe List(NothingIndented(at(1, 3)))
    errors("a { b:\n}") shouldBe List(NothingIndented(at(1, 7)))
    errors("a:\n  b:\n  c: 1") shouldBe List(NothingIndented(at(2, 5)))
    errors("a: @t\nb: 1") shouldBe List(NothingIndented(at(1, 6)))
    // a fatal error below it is all that is reported
    errors("a:\n  \"x") shouldBe List(UnterminatedString(at(2, 3), "\""))
  }

  "A second value, or one among fields," should "be an error" in {
    errors("a:\n  x\n  y") shouldBe List(SecondValue(at(3, 3)))
    errors("a:\n  x, y") shouldBe List(SecondValue(at(2, 6)))
    errors("a:\n  x\n  y: 1\n  z") shouldBe List(ValueAmongFields(at(2, 3)), ValueAmongFields(at(4, 3)))
    errors("m { x: 1 }\na:\n  $m\n  y: 1") shouldBe List(SubstitutionAmongFields(at(3, 3), "m", optional = false))
    errors("a:\n  y: 1\n  #d(x)*") shouldBe List(InjectionOutOfPlace(at(3, 3)))
    errors("a:\n  y: 1\n  @t #d(x)") shouldBe List(ValueAmongFields(at(3, 3)))
    errors("a:\n  #d(x)*") shouldBe List(InjectionOutOfPlace(at(2, 3)), UnknownDirective(at(2, 3), "d"))
    errors("a:\n  x y") shouldBe List(SpaceInValue(at(2, 5), "y"))
  }

  "A tag before the first field of a map" should "be an error that shows where it goes" in {
    errors("a:\n  @t b: 1") shouldBe List(TagBeforeField(at(2, 3)))
    errors("a:\n  - @t @u b: 1") shouldBe List(TagBeforeField(at(2, 5)))
  }

  "Lines" should "start at the width of their block, the top level at column 1" in {
    errors(" a: 1") shouldBe List(UnexpectedIndent(at(1, 2)))
    errors("a: 1\n  b: 2\nc: 3") shouldBe List(UnexpectedIndent(at(2, 3)))
    errors("a:\n    b: 1\n  c: 2\nd: 3") shouldBe List(MisalignedIndent(at(3, 3)))
    // every line is reported until one lands on a width again
    errors("a:\n    b: 1\n  c: 2\n   d: 3\ne: 4") shouldBe List(MisalignedIndent(at(3, 3)), UnexpectedIndent(at(4, 4)))
    // a line deeper than its block reports what it would have opened with it
    errors("a: 1\n  b:\n    c: 2") shouldBe List(UnexpectedIndent(at(2, 3)), UnexpectedIndent(at(3, 5)))
  }

  "A `{…}`" should "take the width of its first line that starts in it, and a `}` in any column" in {
    shown("a {\n  x: 1\n  y: 2\n}") shouldBe "a:\n  x: 1\n  y: 2\n"
    shown("a { x: 1\n    y: 2\n         }") shouldBe "a:\n  x: 1\n  y: 2\n"
    shown("a {\nx: 1\n}") shouldBe "a:\n  x: 1\n"
    shown("a { b:\n      c: 1 }") shouldBe "a:\n  b:\n    c: 1\n"
    errors("a {\n  x: 1\n    y: 2\n}") shouldBe List(UnexpectedIndent(at(3, 5)))
    errors("a {\n    x: 1\n  y: 2\n}") shouldBe List(MisalignedIndent(at(3, 3)))
    // a `}` closes the blocks opened by indentation inside its own
    shown("a {\n  b:\n    c:\n      d: 1 }\ne: 2") shouldBe "a:\n  b:\n    c:\n      d: 1\ne: 2\n"
    shown("a {\n  b:\n    c: 1\n}") shouldBe "a:\n  b:\n    c: 1\n"
  }

  "Inside `[…]` and `(…)`" should "mean nothing, until a `{` opens a block there" in {
    shown("a: [\n      1,\n  2,\n    3]") shouldBe "a:\n  - 1\n  - 2\n  - 3\n"
    shown("a: #env(\n         name: PSI_NO_SUCH_VARIABLE,\n  default: v)") shouldBe "a: v\n"
    shown("a: [{\n  x: 1\n  y:\n    z: 2 }]") shouldBe "a:\n  - x: 1\n    y:\n      z: 2\n"
    errors("a: [\n  - 1\n]") shouldBe List(MissingValue(at(2, 3), "'- '"))
    errors("a: [@t\n]") shouldBe List(MissingValue(at(1, 7), "a line break"))
    errors("a: [{\n  x: 1\n    y: 2 }]") shouldBe List(UnexpectedIndent(at(3, 5)))
    // deeper than the line of its key, the first that starts in the `{…}` being further on
    shown("a: [{ b:\n  c: 1 }]") shouldBe "a:\n  - b:\n      c: 1\n"
  }

  "A `- `" should "start an element at the start of a line, deeper than its key" in {
    shown("a:\n  - 1\n  - x\n  -   \"y\"\n  - [2]\n  - {}") shouldBe "a:\n  - 1\n  - x\n  - y\n  - - 2\n  - {}\n"
    shown("a:\n  - -1\n  - - 5") shouldBe "a:\n  - -1\n  - - 5\n"
    errors("a:\n- 1") shouldBe List(NothingIndented(at(1, 3)), ExpectedKey(at(2, 1), "'- '"))
  }

  it should "hold a map, its first field on the line of the `- `, the rest aligned under it" in {
    shown("servers:\n  - host: a\n    port: 1\n  - host: b\n    port: 2") shouldBe
      "servers:\n  - host: a\n    port: 1\n  - host: b\n    port: 2\n"
    errors("a:\n  - host: a\n   port: 1") shouldBe List(MisalignedIndent(at(3, 4)))
    errors("a:\n  - host: a\n     port: 1") shouldBe List(UnexpectedIndent(at(3, 6)))
  }

  it should "hold a sequence, `- - x` as in YAML" in {
    shown("a:\n  - - x\n    - y\n  - z") shouldBe "a:\n  - - x\n    - y\n  - z\n"
    shown("a:\n  - - - x\n      - y") shouldBe "a:\n  - - - x\n      - y\n"
  }

  it should "take a value in a block below it, deeper than the marker, alone or after tags" in {
    shown("a:\n  - @t\n    host: a\n  -\n    host: b\n  - @u\n   - 1") shouldBe
      "a:\n  - @t\n    host: a\n  - host: b\n  - @u\n    - 1\n"
    errors("a:\n  -\n  - 1") shouldBe List(NothingIndented(at(2, 4)))
    errors("a:\n  - @t\n  - 1") shouldBe List(NothingIndented(at(2, 7)))
    errors("a:\n  -") shouldBe List(MissingValue(at(2, 4), "the end of the source"))
    errors("a:\n  - , b") shouldBe List(MissingValue(at(2, 5), "','"), MixedBlock(at(2, 7)))
  }

  it should
    "graft with `$a` and inject with `$a*`, as in `[…]`, a map element starting with one having a field after it" in {
      shown("s: [1, 2]\nm { x: 1 }\na:\n  - $s\n  - $s*\n  - $m*\n    y: 2\n  - { $m* }") shouldBe
        "s:\n  - 1\n  - 2\nm:\n  x: 1\na:\n  - - 1\n    - 2\n  - 1\n  - 2\n  - x: 1\n    y: 2\n  - x: 1\n"
      errors("s: [1]\na:\n  - @t\n    $s*") shouldBe List(TaggedInjection(at(4, 5)))
      errors("a:\n  - @t\n    #d(x)*") shouldBe List(TaggedInjection(at(3, 5)), UnknownDirective(at(3, 5), "d"))
    }

  it should "not stand among fields, nor fields among elements" in {
    errors("a:\n  - x\n  y: 1") shouldBe List(MixedBlock(at(3, 3)))
    errors("a:\n  y: 1\n  - x") shouldBe List(MixedBlock(at(3, 3)))
    errors("a:\n  x\n  - y") shouldBe List(MixedBlock(at(3, 3)))
  }

  "Indentation" should "count against the depth limit" in {
    val by = Psi.builder.withDepthLimit(1).build.fold(e => fail(e.toString), identity)
    by.parse("s", "a:\n  b:\n    c: 1").fold(_.errors, root => fail(root.toString)) shouldBe
      List(DepthExceeded(at(3, 5), 1, fatal = true))
    by.parse("s", "a:\n  - - 1").fold(_.errors, root => fail(root.toString)) shouldBe
      List(DepthExceeded(at(2, 5), 1, fatal = true))
    by.parse("s", "a:\n  b: 1").isRight shouldBe true
  }
}
