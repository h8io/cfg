package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpreadTest extends AnyFlatSpec with Matchers {
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

  "A spread among the fields of a block" should "merge the fields of the map there, as if written there" in {
    shown("a {\n  x: 1\n  y: 2\n}\nb {\n  $a\n  x: 3\n  z: 4\n}", "b") shouldBe "{\n  x: 3\n  y: 2\n  z: 4\n}"
    // what stands before it, it overrides
    shown("a { x: 1, y: 2 }\nb { x: 3, $a }", "b") shouldBe "{\n  x: 1\n  y: 2\n}"
  }

  it should "be written with braces or without, a '*' or none" in {
    for (spread <- List("$a", "$a*", "${a}", "${a*}"))
      shown("a { x: 1 }\nb { " + spread + ", y: 2 }", "b") shouldBe "{\n  x: 1\n  y: 2\n}"
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

  it should "take the map as it ends up, merged from every source, and what is merged after the spread too" in {
    shown("a { x: 1 }\nb { $a }\na.y: 2\nb { z: 3 }\nb.x: 4", "b") shouldBe "{\n  x: 4\n  y: 2\n  z: 3\n}"
  }

  it should "spread a map that holds spreads of its own, and one into a block that holds its own" in {
    shown("a { x: 1 }\nm { $a, y: 2 }\nn { $m, z: 3 }", "n") shouldBe "{\n  x: 1\n  y: 2\n  z: 3\n}"
    // `c` has a spread of its own when `$a` brings a `c` in, which is merged after it
    shown("a { c { u: 1 } }\nd { c { v: 2 } }\nb { c { $d.c }, $a }", "b") shouldBe
      "{\n  c {\n    v: 2\n    u: 1\n  }\n}"
  }

  it should "not stand at the root, which every path goes through, nor refer to the block it stands in" in {
    errors("a { x: 1 }\n$a\nb: 2\n$?c") shouldBe List(SpreadAtRoot(at(2, 1)), SpreadAtRoot(at(4, 1)))
    errors("m { a { x: 1 }, $m.a }") shouldBe List(SubstitutionCycle(at(1, 17), "m.a"))
  }

  it should "stand in the block a source with one at its top is included in" in {
    val file = java.nio.file.Files.createTempFile("spread", ".psi")
    try {
      java.nio.file.Files.write(file, "$a\ny: 2".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      shown("a { x: 1 }\nb { #include(url: \"" + file.toUri.toString + "\") }", "b") shouldBe "{\n  x: 1\n  y: 2\n}"
    } finally java.nio.file.Files.delete(file)
  }

  it should "be looked into by a path, and find nothing when optional and missing" in {
    value(loaded("a { x: 1 }\nb { $a }\nc: $b.x")("c")) shouldBe "1"
    // a field may refer to another of the same block, the spread merged before either is resolved
    shown("a { x: 1 }\nm { $a, y: $m.x }", "m") shouldBe "{\n  x: 1\n  y: 1\n}"
    shown("b { $?nope, y: 1 }", "b") shouldBe "{\n  y: 1\n}"
  }

  "A spread among the elements of a sequence" should "put the elements of the sequence there" in {
    shown("a: [x, y]\nb: [a, $a*, z]", "b") shouldBe "[a, x, y, z]"
    shown("a: [x, y]\nb: [a, ${a*}, z]", "b") shouldBe "[a, x, y, z]"
    // without a '*', a substitution grafts the sequence whole
    shown("a: [x, y]\nb: [a, $a, z]", "b") shouldBe "[\n  a\n  [x, y]\n  z\n]"
    shown("b: [a, $?nope*, z]", "b") shouldBe "[a, z]"
    value(loaded("a: [x, y]\nb: [a, $a*, z]\nc: $b[2]")("c")) shouldBe "y"
  }

  "A spread of what it cannot spread" should "be an error naming what it found" in {
    errors("a: 5\nb { $a }") shouldBe List(SpreadNotMap(at(2, 5), "a", at(1, 4)))
    errors("a { x: 1 }\nb: [$a*]") shouldBe List(SpreadNotSequence(at(2, 5), "a", at(1, 3)))
    errors("b { $nope }") shouldBe List(UnresolvedSubstitution(at(1, 5), "nope"))
    errors("b { $b }") shouldBe List(SubstitutionCycle(at(1, 5), "b"))
  }

  "A spread where it has nothing to put its contents among" should "be an error" in {
    errors("a: [1]\nb: $a*") shouldBe List(MisplacedSpread(at(2, 4)))
    errors("a: [1]\nb := ${a*}") shouldBe List(MisplacedSpread(at(2, 6)))
    errors("a: 1\nb: \"x${a*}\"") shouldBe List(MisplacedSpread(at(2, 6)))
    errors("a: 1\nb: x$a*") shouldBe List(MisplacedSpread(at(2, 5)))
    errors("a { x: 1 }\nb { $a: 1 }") shouldBe List(MisplacedSpread(at(2, 5)))
    errors("a: [1]\nb: [@t $a*]") shouldBe List(TaggedSpread(at(2, 8)))
    // and is written as it is where an error quotes it
    errors("a.$b*: 1\nc.$?d*: 2") shouldBe
      List(InvalidKey(at(1, 3), "$" + "{b*}:"), InvalidKey(at(2, 3), "$" + "{?d*}:"))
  }

  "What a spread brings in" should "not merge a map over a substitution, which may be no map" in {
    errors("a { c { p: 1 } }\nx { q: 2 }\nb { c: $x, $a }") shouldBe
      List(MergeWithSubstitution(ReferenceLocation(at(3, 12), "a.c", at(1, 7)), at(3, 8)))
    // what is written after the spread merges as it is written, its errors included
    errors("a { c { p: 1 } }\nx { q: 2 }\nb { $a, c: $x, c { r: 3 } }") shouldBe
      List(MergeWithSubstitution(at(3, 12), ReferenceLocation(at(3, 5), "a.c", at(1, 7))))
  }

  it should "fit under the depth limit, and count as the nodes it copies" in {
    // resolved for `r` first, a path looked up with no step for its levels, the spread's map lands 6 levels down
    val deep = "r: $x1.x2.x3.x4.x5.c\nx1 { x2 { x3 { x4 { x5 { c "
    errors(deep + "{ $a } } } } } }\na { p { q: 1 } }", loader(depth = 6)) shouldBe
      List(DepthExceeded(at(2, 30), 6, fatal = false))
    errors(deep + ": [$a*] } } } } }\na: [[1]]", loader(depth = 6)) shouldBe
      List(DepthExceeded(at(2, 31), 6, fatal = false))
    errors("a { x: 1, y: 2 }\nb { $a }", loader(nodes = 1)) shouldBe List(ResolutionTooLarge(at(2, 5), "a", 1))
    errors("a: [1, 2]\nb: [$a*]", loader(nodes = 1)) shouldBe List(ResolutionTooLarge(at(2, 5), "a", 1))
  }

  "A tree with a spread in a sequence" should "be written back as it is written" in {
    val parsed = Parser.parse(Source("s", "a: [1]\nb: [${a*}]"))
    Render.document(Merge(::((parsed.value, at(1, 1)), Nil)).value) shouldBe "a: [1]\nb: [${a*}]\n"
  }

  "Its errors" should "say what is wrong, and what to do" in {
    (MisplacedSpread(at(1, 1)).message, MisplacedSpread(at(1, 1)).hint) shouldBe
      ((
        "a spread stands alone, among the fields of a block or the elements of a sequence",
        Some("to graft the node whole, write the substitution without '*'")))
    (SpreadAtRoot(at(1, 1)).message, SpreadAtRoot(at(1, 1)).hint) shouldBe
      ((
        "a spread at the root, which every path goes through, refers to the tree it builds",
        Some("spread into a block, or include the source at the root with #include")))
    (TaggedSpread(at(1, 1)).message, TaggedSpread(at(1, 1)).hint) shouldBe
      (("a tag before a spread, which puts elements with tags of their own", Some("remove the tag")))
    SpreadNotMap(at(1, 1), "a", at(2, 3)).message shouldBe
      "'$" + "{a}' spread among fields is not a map; it is defined at s:2:3"
    SpreadNotSequence(at(1, 1), "a", at(2, 3)).message shouldBe
      "'$" + "{a}' spread among elements is not a sequence; it is defined at s:2:3"
  }
}
