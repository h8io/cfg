package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InjectionTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def loader(depth: Int = PsiLoader.DefaultDepthLimit, nodes: Int = PsiLoader.DefaultNodeLimit): PsiLoader =
    Psi.builder.withDepthLimit(depth).withSizeLimit(nodes, PsiLoader.DefaultCharacterLimit).build
      .fold(e => fail(e.toString), identity)

  private def loaded(text: String): Node.IMap[Id.Root] = loader().parse("s", text).fold(e => fail(e.toString), identity)

  private def errors(text: String, by: PsiLoader = loader()): List[PsiError] =
    by.parse("s", text).fold(_.errors, root => fail(root.toString))

  private def value(node: Node): String =
    node match {
      case Node.IScalar(_, _, value, _) => value
      case other => fail(other.toString)
    }

  /** The node at `key` as Psi. */
  private def shown(text: String, key: String): String = loaded(text)(key).toString

  "An injection among the fields of a block" should "merge the fields of the map there, as if written there" in {
    shown("a {\n  x: 1\n  y: 2\n}\nb {\n  $a*\n  x: 3\n  z: 4\n}", "b") shouldBe "x: 3\ny: 2\nz: 4"
    // what stands before it, it overrides
    shown("a { x: 1, y: 2 }\nb { x: 3, $a* }", "b") shouldBe "x: 1\ny: 2"
  }

  it should "be written with braces or without" in {
    for (injection <- List("$a*", "${a*}"))
      shown("a { x: 1 }\nb { " + injection + ", y: 2 }", "b") shouldBe "x: 1\ny: 2"
  }

  it should "keep where each field came from, its own tags and none of the map's" in {
    loaded("a: @t { x: @u 1 }\nb { $a* }")("b") match {
      case map: Node.IMap[?] =>
        map.tags shouldBe Set.empty
        map("x") shouldBe
          Node.IScalar(Id.Key("x", Id.Key("b", Id.Root)), Set("u"), "1", ReferenceLocation(at(2, 5), "a.x", at(1, 15)))
      case other => fail(other.toString)
    }
  }

  it should "merge deep, as ':' does, and replace where ':=' does" in {
    val a = "a { c { p: 1, q: 2 } }\n"
    shown(a + "b { $a*, c { q: 3 } }", "b") shouldBe "c:\n  p: 1\n  q: 3"
    shown(a + "b { $a*, c := { q: 3 } }", "b") shouldBe "c:\n  q: 3"
    // a block written before it merges with the one it brings in
    shown(a + "b { c { r: 0 }, $a* }", "b") shouldBe "c:\n  r: 0\n  p: 1\n  q: 2"
    // a value that is no map replaces one, and the other way round
    shown(a + "b { c: 5, $a* }", "b") shouldBe "c:\n  p: 1\n  q: 2"
  }

  it should "take the map as it ends up, merged from every source, and what is merged after the injection too" in {
    shown("a { x: 1 }\nb { $a* }\na.y: 2\nb { z: 3 }\nb.x: 4", "b") shouldBe "x: 4\ny: 2\nz: 3"
  }

  it should "inject a map that holds injections of its own, and one into a block that holds its own" in {
    shown("a { x: 1 }\nm { $a*, y: 2 }\nn { $m*, z: 3 }", "n") shouldBe "x: 1\ny: 2\nz: 3"
    // `c` has an injection of its own when `$a` brings a `c` in, which is merged after it
    shown("a { c { u: 1 } }\nd { c { v: 2 } }\nb { c { $d.c* }, $a* }", "b") shouldBe
      "c:\n  v: 2\n  u: 1"
  }

  it should "not stand at the root, which every path goes through, nor refer to the block it stands in" in {
    errors("a { x: 1 }\n$a*\nb: 2\n$?c*") shouldBe List(InjectionAtRoot(at(2, 1)), InjectionAtRoot(at(4, 1)))
    errors("m { a { x: 1 }, $m.a* }") shouldBe List(SubstitutionCycle(at(1, 17), "m.a"))
  }

  it should "stand in the block a source with one at its top is included in" in {
    val file = java.nio.file.Files.createTempFile("injection", ".psi")
    try {
      java.nio.file.Files.write(file, "$a*\ny: 2".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      shown("a { x: 1 }\nb { #include(url: \"" + file.toUri.toString + "\") }", "b") shouldBe "x: 1\ny: 2"
    } finally java.nio.file.Files.delete(file)
  }

  it should "be looked into by a path, and find nothing when optional and missing" in {
    value(loaded("a { x: 1 }\nb { $a* }\nc: $b.x")("c")) shouldBe "1"
    // a field may refer to another of the same block, the injection merged before either is resolved
    shown("a { x: 1 }\nm { $a*, y: $m.x }", "m") shouldBe "x: 1\ny: 1"
    shown("b { $?nope*, y: 1 }", "b") shouldBe "y: 1"
  }

  "An injection among the elements of a sequence" should "put the elements of a sequence there" in {
    shown("a: [x, y]\nb: [a, $a*, z]", "b") shouldBe "- a\n- x\n- y\n- z"
    shown("a: [x, y]\nb: [a, ${a*}, z]", "b") shouldBe "- a\n- x\n- y\n- z"
    shown("a: []\nb: [a, $a*, z]", "b") shouldBe "- a\n- z"
    shown("b: [a, $?nope*, ${?nope*}, z]", "b") shouldBe "- a\n- z"
  }

  "A substitution among the elements of a sequence with no '*'" should "stand there as one element" in {
    shown("a: [x, y]\nb: [a, $a, z]", "b") shouldBe "- a\n- - x\n  - y\n- z"
    shown("a: x\nb: [a, $a, z]", "b") shouldBe "- a\n- x\n- z"
    shown("a: @u x\nb: [@t $a]", "b") shouldBe "- @t @u x"
    shown("b: [a, $?nope, z]", "b") shouldBe "- a\n- z"
  }

  "An injection among elements that finds no sequence" should "be an error naming what it found" in {
    errors("a: x\nb: [$a*]") shouldBe List(InjectionNotSequence(at(2, 5), "a", at(1, 4)))
    errors("a { x: 1 }\nb: [$a*]") shouldBe List(InjectionNotSequence(at(2, 5), "a", at(1, 3)))
    errors("a: null\nb: [p, $a*]") shouldBe List(InjectionNotSequence(at(2, 8), "a", at(1, 4)))
  }

  "An index into a sequence" should "count what its injections put, and resolve only what stands before" in {
    val text = "a: [x, y]\nb: [p, $a*, $?nope*, q, $c]\nc: r\n"
    value(loaded(text + "d: $b[2]")("d")) shouldBe "y"
    value(loaded(text + "d: $b[3]")("d")) shouldBe "q"
    value(loaded(text + "d: $b[4]")("d")) shouldBe "r"
    errors(text + "d: $b[5]") shouldBe List(UnresolvedSubstitution(at(4, 4), "b[5]"))
    errors("a: x\nb: [$a*, q]\nd: $b[0]") shouldBe List(InjectionNotSequence(at(2, 5), "a", at(1, 4)))
    // an element refers to one before it in its own sequence
    shown("a: [x, $a[0]]", "a") shouldBe "- x\n- x"
    shown("a: [[x, y], $a[0]*]", "a") shouldBe "- - x\n  - y\n- x\n- y"
    shown("a: [x, $b*, $a[0]]\nb: [y]", "a") shouldBe "- x\n- y\n- x"
  }

  "An injection" should "take no tags, which would have no node to go to" in {
    errors("a: [1]\nb: [@t $a*]") shouldBe List(TaggedInjection(at(2, 8)))
  }

  it should "stand nowhere but alone among fields or elements" in {
    errors("a { x: 1 }\nb: $a*") shouldBe List(InjectionOutOfPlace(at(2, 4)))
    errors("a { x: 1 }\nb: ${a*}") shouldBe List(InjectionOutOfPlace(at(2, 4)))
    errors("a: x\nb: x$a*") shouldBe List(InjectionOutOfPlace(at(2, 4)))
    errors("a: x\nb: \"${a*}\"") shouldBe List(InjectionOutOfPlace(at(2, 4)))
    errors("a: x\nb: [\"-${a*}\"]") shouldBe List(InjectionOutOfPlace(at(2, 5)))
    // a `*` after braces is text, and in quotes `$a*` is
    value(loaded("a: x\nb: ${a}*")("b")) shouldBe "x*"
    value(loaded("b: \"$a*\"")("b")) shouldBe "$a*"
  }

  "A substitution alone among the fields of a block, with no '*'" should "be an error that says to add it" in {
    errors("a { x: 1 }\nb { $a }") shouldBe List(SubstitutionAmongFields(at(2, 5), "a", optional = false))
    errors("b { ${?a} }") shouldBe List(SubstitutionAmongFields(at(1, 5), "a", optional = true))
    SubstitutionAmongFields(at(1, 1), "a", optional = false).message shouldBe
      "'$" + "{a}' among the fields of a block has no key to stand at"
    // a quoted one, which is no key, is written back as it was
    errors("\"$" + "{a*}$" + "{b}\": 1") shouldBe List(QuotedKey(at(1, 1), "$" + "{a*}$" + "{b}"))
  }

  "A directive among the elements of a sequence" should "put the elements of a sequence it gives there" in {
    val seq = Psi.parse("d", "s: [p, @u q]").fold(e => fail(e.toString), _("s")) match {
      case seq: Node.ISeq[?] => seq
      case other => fail(other.toString)
    }
    val scalar = Psi.parse("d", "s: v").fold(e => fail(e.toString), _("s")) match {
      case scalar: Node.IScalar[?] => scalar
      case other => fail(other.toString)
    }
    def giving(called: String, node: Node.ISome[Id]): Directive =
      new Directive {
        def name: String = called
        def parameters: Seq[Parameter] = Nil
        def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Option[Fragment]] =
          Right(Some(Fragment(node)))
      }
    val parsing = new Directive {
      def name: String = "parsed"
      def parameters: Seq[Parameter] = Nil
      def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Option[Fragment]] =
        Right(Some(context.parse("p", "x: 1")))
    }
    val by = Psi.builder.withDirectives(giving("seq", seq), giving("one", scalar), parsing).build
      .fold(e => fail(e.toString), identity)
    def shown(text: String): String = by.parse("s", text).fold(e => fail(e.toString), _("b").toString)
    // what it puts stands where it does, under the depth limit
    val nested = Psi.parse("d", "s: [[p]]").fold(e => fail(e.toString), _("s")) match {
      case found: Node.ISeq[?] => found
      case other => fail(other.toString)
    }
    Psi.builder.withDirectives(giving("seq", nested)).withDepthLimit(1).build
      .fold(e => fail(e.toString), identity)
      .parse("s", "b: [#seq*]")
      .fold(_.errors, root => fail(root.toString)) shouldBe List(DepthExceeded(at(1, 5), 1, fatal = false))
    shown("b: [a, #seq*, z]") shouldBe "- a\n- p\n- @u q\n- z"
    shown("b: [a, #seq()*, z]") shouldBe "- a\n- p\n- @u q\n- z"
    // with no `*` what it gives stands as one element, as a field's value
    shown("b: [a, #seq, z]") shouldBe "- a\n- - p\n  - @u q\n- z"
    shown("b: [a, @t #one, z]") shouldBe "- a\n- @t v\n- z"
    shown("b: #seq") shouldBe "- p\n- @u q"
    def errors(text: String): List[PsiError] = by.parse("s", text).fold(_.errors, root => fail(root.toString))
    errors("b: [@t #seq*]") shouldBe List(TaggedInjection(at(1, 8)))
    errors("b: [#one*]") shouldBe List(DirectiveNotSequence(at(1, 5), "one"))
    errors("b: [#parsed*]") shouldBe List(DirectiveNotSequence(at(1, 5), "parsed"))
    // anywhere but among elements a `*` after a call is an error
    errors("b: #seq*") shouldBe List(InjectionOutOfPlace(at(1, 8)))
    errors("b { #seq* }") shouldBe List(InjectionOutOfPlace(at(1, 9)))
    // a `*` apart from the call is a second value
    errors("b: [#seq *]") shouldBe List(MissingSeparator(at(1, 10), "'*'"))
    // a call with an error in it is left out, its `*` with it
    errors("b: [#seq(x y)*, z]").map(_.getClass) shouldBe List(classOf[MissingSeparator])
  }

  "Injections into one block" should "each report their own errors" in {
    errors("b { $missing1*, x: 1, $missing2* }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing1"), UnresolvedSubstitution(at(1, 23), "missing2"))
    // the injections after one that failed still report theirs, and nothing is merged past what it would have brought
    errors("x { q: 1 }\nb { $missing*, $x*, $nope* }") shouldBe
      List(UnresolvedSubstitution(at(2, 5), "missing"), UnresolvedSubstitution(at(2, 21), "nope"))
    errors("x { q: 1 }\nb { c: $x, $missing*, c { r: 2 } }") shouldBe
      List(UnresolvedSubstitution(at(2, 12), "missing"))
    errors("a { c { u: 1 } }\nb { c { $missing* }, $a* }") shouldBe List(UnresolvedSubstitution(at(2, 9), "missing"))
  }

  it should "leave the fields after one that failed to report their own errors" in {
    errors("b { $missing*, x := $other }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing"), UnresolvedSubstitution(at(1, 21), "other"))
    errors("b { $missing*, x { y: $other } }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing"), UnresolvedSubstitution(at(1, 23), "other"))
    // a field referring into the map, which failed, fails with it
    errors("b { $missing*, x: $b.y, y: 1 }") shouldBe List(UnresolvedSubstitution(at(1, 5), "missing"))
    // the fields between two injections are merged apart: what the one between would have brought is unknown
    errors("a { x { p: 1 } }\nb { $missing*, x: $a, $a*, x { q: 2 } }") shouldBe
      List(UnresolvedSubstitution(at(2, 5), "missing"))
    // what is written after it merges as it is written, its errors included
    errors("b { $missing*, x: $a, x { q: 2 } }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing"), MergeWithSubstitution(at(1, 25), at(1, 19)))
  }

  "A block with an injection of its own, merged with one an injection brings" should "keep where each came from" in {
    loaded("a { c { u: 1 } }\nd { c { v: 2 } }\nb { c { $d.c* }, $a* }")("b") match {
      case b: Node.IMap[?] =>
        b("c") match {
          case c: Node.IMap[?] =>
            c.location shouldBe MergedLocation(ReferenceLocation(at(3, 18), "a.c", at(1, 7)), ::(at(3, 7), Nil))
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
  }

  "An injection of a field's fields that is no map" should "be an error naming what it found" in {
    errors("a: 5\nb { $a* }") shouldBe List(InjectionNotMap(at(2, 5), "a", at(1, 4)))
    errors("b { $nope* }") shouldBe List(UnresolvedSubstitution(at(1, 5), "nope"))
    errors("b { $b* }") shouldBe List(SubstitutionCycle(at(1, 5), "b"))
  }

  "An injection among the fields of a block with more than the substitution written" should "be an error" in {
    errors("a { x: 1 }\nb { $a: 1 }") shouldBe List(MisplacedInjection(at(2, 5)))
    errors("a { x: 1 }\nb { $a/x }") shouldBe List(MisplacedInjection(at(2, 5)))
    // nor is a key ever computed
    errors("a.$b: 1") shouldBe List(InvalidKey(at(1, 3), "$" + "{b}:"))
  }

  "What an injection brings in" should "not merge a map over a substitution, which may be no map" in {
    errors("a { c { p: 1 } }\nx { q: 2 }\nb { c: $x, $a* }") shouldBe
      List(MergeWithSubstitution(ReferenceLocation(at(3, 12), "a.c", at(1, 7)), at(3, 8)))
    // what is written after the injection merges as it is written, its errors included
    errors("a { c { p: 1 } }\nx { q: 2 }\nb { $a*, c: $x, c { r: 3 } }") shouldBe
      List(MergeWithSubstitution(at(3, 13), ReferenceLocation(at(3, 5), "a.c", at(1, 7))))
  }

  it should "fit under the depth limit, and count as the nodes it copies" in {
    // resolved for `r` first, a path looked up with no step for its levels, the injection's map lands 6 levels down
    val deep = "r: $x1.x2.x3.x4.x5.c\nx1 { x2 { x3 { x4 { x5 { c "
    errors(deep + "{ $a* } } } } } }\na { p { q: 1 } }", loader(depth = 6)) shouldBe
      List(DepthExceeded(at(2, 30), 6, fatal = false))
    errors(deep + ": [$a*] } } } } }\na: [[1]]", loader(depth = 6)) shouldBe
      List(DepthExceeded(at(2, 31), 6, fatal = false))
    errors("a { x: 1, y: 2 }\nb { $a* }", loader(nodes = 1)) shouldBe List(ResolutionTooLarge(at(2, 5), "a", 1))
    errors("a: [1, 2]\nb: [$a*]", loader(nodes = 1)) shouldBe List(ResolutionTooLarge(at(2, 5), "a", 1))
  }

  "A tree with an injection in a sequence" should "be written back as it is written" in {
    val parsed = Parser.parse(Source("s", "a: [1]\nb: [$a*, @t ${?c}, $?d*]"))
    Render.document(Merge(::((parsed.value, at(1, 1)), Nil)).value) shouldBe
      "a:\n  - 1\nb:\n  - ${a*}\n  - @t ${?c}\n  - ${?d*}\n"
  }

  "Its errors" should "say what is wrong, and what to do" in {
    (MisplacedInjection(at(1, 1)).message, MisplacedInjection(at(1, 1)).hint) shouldBe
      (("an injection stands alone among the fields of a block", Some("write a key and ':' before a value")))
    (InjectionAtRoot(at(1, 1)).message, InjectionAtRoot(at(1, 1)).hint) shouldBe
      ((
        "an injection at the root, which every path goes through, refers to the tree it builds",
        Some("inject into a block, or include the source at the root with #include")))
    InjectionNotMap(at(1, 1), "a", at(2, 3)).message shouldBe
      "'$" + "{a}' injected among fields is not a map; it is defined at s:2:3"
    (TaggedInjection(at(1, 1)).message, TaggedInjection(at(1, 1)).hint) shouldBe
      ((
        "a tag before an injection, whose elements keep their own tags",
        Some("remove the tag, or the '*' to put the node itself as one element")))
    (InjectionNotSequence(at(1, 1), "a", at(2, 3)).message, InjectionNotSequence(at(1, 1), "a", at(2, 3)).hint) shouldBe
      ((
        "'$" + "{a}' injected among elements is not a sequence; it is defined at s:2:3",
        Some("remove the '*' to put it as one element")))
    (DirectiveNotSequence(at(1, 1), "d").message, DirectiveNotSequence(at(1, 1), "d").hint) shouldBe
      ((
        "'#d' injected among elements gave something other than a sequence",
        Some("remove the '*' to put what it gives as one element")))
    (InjectionOutOfPlace(at(1, 1)).message, InjectionOutOfPlace(at(1, 1)).hint) shouldBe
      ((
        "an injection stands alone among the fields of a block or the elements of a sequence",
        Some("remove the '*' to put the node itself here, or quote a '*' that is text")))
    val among = SubstitutionAmongFields(at(1, 1), "a", optional = true)
    (among.message, among.hint) shouldBe
      ((
        "'$" + "{?a}' among the fields of a block has no key to stand at",
        Some("write '$" + "{?a*}' to inject the fields of the map there, or a key and ':' before it")))
  }
}
