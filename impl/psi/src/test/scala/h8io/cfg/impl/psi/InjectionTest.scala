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
    shown("a {\n  x: 1\n  y: 2\n}\nb {\n  $a\n  x: 3\n  z: 4\n}", "b") shouldBe "{\n  x: 3\n  y: 2\n  z: 4\n}"
    // what stands before it, it overrides
    shown("a { x: 1, y: 2 }\nb { x: 3, $a }", "b") shouldBe "{\n  x: 1\n  y: 2\n}"
  }

  it should "be written with braces or without" in {
    for (injection <- List("$a", "${a}"))
      shown("a { x: 1 }\nb { " + injection + ", y: 2 }", "b") shouldBe "{\n  x: 1\n  y: 2\n}"
  }

  it should "keep where each field came from, its own tags and none of the map's" in {
    loaded("a: @t { x: @u 1 }\nb { $a }")("b") match {
      case map: Node.IMap[?] =>
        map.tags shouldBe Set.empty
        map("x") shouldBe
          Node.IScalar(Id.Key("x", Id.Key("b", Id.Root)), Set("u"), "1", ReferenceLocation(at(2, 5), "a.x", at(1, 15)))
      case other => fail(other.toString)
    }
  }

  it should "merge deep, as ':' does, and replace where ':=' does" in {
    val a = "a { c { p: 1, q: 2 } }\n"
    shown(a + "b { $a, c { q: 3 } }", "b") shouldBe "{\n  c {\n    p: 1\n    q: 3\n  }\n}"
    shown(a + "b { $a, c := { q: 3 } }", "b") shouldBe "{\n  c {\n    q: 3\n  }\n}"
    // a block written before it merges with the one it brings in
    shown(a + "b { c { r: 0 }, $a }", "b") shouldBe "{\n  c {\n    r: 0\n    p: 1\n    q: 2\n  }\n}"
    // a value that is no map replaces one, and the other way round
    shown(a + "b { c: 5, $a }", "b") shouldBe "{\n  c {\n    p: 1\n    q: 2\n  }\n}"
  }

  it should "take the map as it ends up, merged from every source, and what is merged after the injection too" in {
    shown("a { x: 1 }\nb { $a }\na.y: 2\nb { z: 3 }\nb.x: 4", "b") shouldBe "{\n  x: 4\n  y: 2\n  z: 3\n}"
  }

  it should "inject a map that holds injections of its own, and one into a block that holds its own" in {
    shown("a { x: 1 }\nm { $a, y: 2 }\nn { $m, z: 3 }", "n") shouldBe "{\n  x: 1\n  y: 2\n  z: 3\n}"
    // `c` has an injection of its own when `$a` brings a `c` in, which is merged after it
    shown("a { c { u: 1 } }\nd { c { v: 2 } }\nb { c { $d.c }, $a }", "b") shouldBe
      "{\n  c {\n    v: 2\n    u: 1\n  }\n}"
  }

  it should "not stand at the root, which every path goes through, nor refer to the block it stands in" in {
    errors("a { x: 1 }\n$a\nb: 2\n$?c") shouldBe List(InjectionAtRoot(at(2, 1)), InjectionAtRoot(at(4, 1)))
    errors("m { a { x: 1 }, $m.a }") shouldBe List(SubstitutionCycle(at(1, 17), "m.a"))
  }

  it should "stand in the block a source with one at its top is included in" in {
    val file = java.nio.file.Files.createTempFile("injection", ".psi")
    try {
      java.nio.file.Files.write(file, "$a\ny: 2".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      shown("a { x: 1 }\nb { #include(url: \"" + file.toUri.toString + "\") }", "b") shouldBe "{\n  x: 1\n  y: 2\n}"
    } finally java.nio.file.Files.delete(file)
  }

  it should "be looked into by a path, and find nothing when optional and missing" in {
    value(loaded("a { x: 1 }\nb { $a }\nc: $b.x")("c")) shouldBe "1"
    // a field may refer to another of the same block, the injection merged before either is resolved
    shown("a { x: 1 }\nm { $a, y: $m.x }", "m") shouldBe "{\n  x: 1\n  y: 1\n}"
    shown("b { $?nope, y: 1 }", "b") shouldBe "{\n  y: 1\n}"
  }

  "An injection among the elements of a sequence" should "put the elements of a sequence there" in {
    shown("a: [x, y]\nb: [a, $a, z]", "b") shouldBe "[a, x, y, z]"
    shown("a: [x, y]\nb: [a, ${a}, z]", "b") shouldBe "[a, x, y, z]"
    shown("a: []\nb: [a, $a, z]", "b") shouldBe "[a, z]"
    shown("b: [a, $?nope, z]", "b") shouldBe "[a, z]"
    // a sequence written around it nests the sequence whole
    shown("a: [x, y]\nb: [a, [$a], z]", "b") shouldBe "[\n  a\n  [x, y]\n  z\n]"
  }

  it should "put anything else as one element, with the tags written before it" in {
    shown("a: x\nb: [a, $a, z]", "b") shouldBe "[a, x, z]"
    shown("a: null\nb: [$a]", "b") shouldBe "[null]"
    shown("a { x: 1 }\nb: [$a]", "b") shouldBe "[\n  {\n    x: 1\n  }\n]"
    shown("a: @u x\nb: [@t $a]", "b") shouldBe "[@t @u x]"
  }

  it should "be counted by an index into the sequence, which resolves only what stands before the element" in {
    val text = "a: [x, y]\nb: [p, $a, $?nope, q, $c]\nc: r\n"
    value(loaded(text + "d: $b[2]")("d")) shouldBe "y"
    value(loaded(text + "d: $b[3]")("d")) shouldBe "q"
    value(loaded(text + "d: $b[4]")("d")) shouldBe "r"
    errors(text + "d: $b[5]") shouldBe List(UnresolvedSubstitution(at(4, 4), "b[5]"))
    // an element refers to one before it in its own sequence
    shown("a: [x, $a[0]]", "a") shouldBe "[x, x]"
    shown("a: [[x, y], $a[0]]", "a") shouldBe "[\n  [x, y]\n  x\n  y\n]"
  }

  it should "not take a tag when it puts the elements of a sequence" in {
    errors("a: [1]\nb: [@t $a]") shouldBe List(TaggedInjection(at(2, 8), "a", at(1, 4)))
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
    val by = Psi.builder.withDirectives(giving("seq", seq), giving("one", scalar)).build
      .fold(e => fail(e.toString), identity)
    def shown(text: String): String = by.parse("s", text).fold(e => fail(e.toString), _("b").toString)
    // what it puts stands where it does, under the depth limit
    val nested = Psi.parse("d", "s: [[p]]").fold(e => fail(e.toString), _("s")) match {
      case found: Node.ISeq[?] => found
      case other => fail(other.toString)
    }
    Psi.builder.withDirectives(giving("seq", nested)).withDepthLimit(1).build
      .fold(e => fail(e.toString), identity)
      .parse("s", "b: [#seq]")
      .fold(_.errors, root => fail(root.toString)) shouldBe List(DepthExceeded(at(1, 5), 1, fatal = false))
    shown("b: [a, #seq, z]") shouldBe "[a, p, @u q, z]"
    shown("b: [a, [#seq], z]") shouldBe "[\n  a\n  [p, @u q]\n  z\n]"
    shown("b: [a, @t #one, z]") shouldBe "[a, @t v, z]"
    // as a field's value, the sequence stands whole
    shown("b: #seq") shouldBe "[p, @u q]"
    by.parse("s", "b: [@t #seq]").fold(_.errors, root => fail(root.toString)) shouldBe
      List(TaggedDirectiveInjection(at(1, 8), "seq"))
  }

  "Injections into one block" should "each report their own errors" in {
    errors("b { $missing1, x: 1, $missing2 }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing1"), UnresolvedSubstitution(at(1, 22), "missing2"))
    // the injections after one that failed still report theirs, and nothing is merged past what it would have brought
    errors("x { q: 1 }\nb { $missing, $x, $nope }") shouldBe
      List(UnresolvedSubstitution(at(2, 5), "missing"), UnresolvedSubstitution(at(2, 19), "nope"))
    errors("x { q: 1 }\nb { c: $x, $missing, c { r: 2 } }") shouldBe
      List(UnresolvedSubstitution(at(2, 12), "missing"))
    errors("a { c { u: 1 } }\nb { c { $missing }, $a }") shouldBe List(UnresolvedSubstitution(at(2, 9), "missing"))
  }

  it should "leave the fields after one that failed to report their own errors" in {
    errors("b { $missing, x := $other }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing"), UnresolvedSubstitution(at(1, 20), "other"))
    errors("b { $missing, x { y: $other } }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing"), UnresolvedSubstitution(at(1, 22), "other"))
    // a field referring into the map, which failed, fails with it
    errors("b { $missing, x: $b.y, y: 1 }") shouldBe List(UnresolvedSubstitution(at(1, 5), "missing"))
    // the fields between two injections are merged apart: what the one between would have brought is unknown
    errors("a { x { p: 1 } }\nb { $missing, x: $a, $a, x { q: 2 } }") shouldBe
      List(UnresolvedSubstitution(at(2, 5), "missing"))
    // what is written after it merges as it is written, its errors included
    errors("b { $missing, x: $a, x { q: 2 } }") shouldBe
      List(UnresolvedSubstitution(at(1, 5), "missing"), MergeWithSubstitution(at(1, 24), at(1, 18)))
  }

  "A block with an injection of its own, merged with one an injection brings" should "keep where each came from" in {
    loaded("a { c { u: 1 } }\nd { c { v: 2 } }\nb { c { $d.c }, $a }")("b") match {
      case b: Node.IMap[?] =>
        b("c") match {
          case c: Node.IMap[?] =>
            c.location shouldBe MergedLocation(ReferenceLocation(at(3, 17), "a.c", at(1, 7)), ::(at(3, 7), Nil))
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
  }

  "An injection of a field's fields that is no map" should "be an error naming what it found" in {
    errors("a: 5\nb { $a }") shouldBe List(InjectionNotMap(at(2, 5), "a", at(1, 4)))
    errors("b { $nope }") shouldBe List(UnresolvedSubstitution(at(1, 5), "nope"))
    errors("b { $b }") shouldBe List(SubstitutionCycle(at(1, 5), "b"))
  }

  "An injection among the fields of a block with more than the substitution written" should "be an error" in {
    errors("a { x: 1 }\nb { $a: 1 }") shouldBe List(MisplacedInjection(at(2, 5)))
    errors("a { x: 1 }\nb { $a/x }") shouldBe List(MisplacedInjection(at(2, 5)))
    // nor is a key ever computed
    errors("a.$b: 1") shouldBe List(InvalidKey(at(1, 3), "$" + "{b}:"))
  }

  "What an injection brings in" should "not merge a map over a substitution, which may be no map" in {
    errors("a { c { p: 1 } }\nx { q: 2 }\nb { c: $x, $a }") shouldBe
      List(MergeWithSubstitution(ReferenceLocation(at(3, 12), "a.c", at(1, 7)), at(3, 8)))
    // what is written after the injection merges as it is written, its errors included
    errors("a { c { p: 1 } }\nx { q: 2 }\nb { $a, c: $x, c { r: 3 } }") shouldBe
      List(MergeWithSubstitution(at(3, 12), ReferenceLocation(at(3, 5), "a.c", at(1, 7))))
  }

  it should "fit under the depth limit, and count as the nodes it copies" in {
    // resolved for `r` first, a path looked up with no step for its levels, the injection's map lands 6 levels down
    val deep = "r: $x1.x2.x3.x4.x5.c\nx1 { x2 { x3 { x4 { x5 { c "
    errors(deep + "{ $a } } } } } }\na { p { q: 1 } }", loader(depth = 6)) shouldBe
      List(DepthExceeded(at(2, 30), 6, fatal = false))
    errors(deep + ": [$a] } } } } }\na: [[1]]", loader(depth = 6)) shouldBe
      List(DepthExceeded(at(2, 31), 6, fatal = false))
    errors("a { x: 1, y: 2 }\nb { $a }", loader(nodes = 1)) shouldBe List(ResolutionTooLarge(at(2, 5), "a", 1))
    errors("a: [1, 2]\nb: [$a]", loader(nodes = 1)) shouldBe List(ResolutionTooLarge(at(2, 5), "a", 1))
  }

  "A tree with an injection in a sequence" should "be written back as it is written" in {
    val parsed = Parser.parse(Source("s", "a: [1]\nb: [$a, @t ${?c}]"))
    Render.document(Merge(::((parsed.value, at(1, 1)), Nil)).value) shouldBe "a: [1]\nb: [${a}, @t ${?c}]\n"
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
    (TaggedInjection(at(1, 1), "a", at(2, 3)).message, TaggedInjection(at(1, 1), "a", at(2, 3)).hint) shouldBe
      ((
        "a tag before '$" + "{a}', a sequence injected among elements, whose elements keep their own tags; it is " +
          "defined at s:2:3",
        Some("remove the tag, or nest the sequence whole: [$" + "{a}]")))
    (TaggedDirectiveInjection(at(1, 1), "d").message, TaggedDirectiveInjection(at(1, 1), "d").hint) shouldBe
      ((
        "a tag before '#d', which gave a sequence among elements, whose elements keep their own tags",
        Some("remove the tag, or nest the sequence whole: [#…]")))
  }
}
