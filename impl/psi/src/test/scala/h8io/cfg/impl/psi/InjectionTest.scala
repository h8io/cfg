package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.Registering.*
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

  "A substitution alone among the fields of a block" should "merge the fields of the map there, as if written there" in {
    shown("a {\n  x: 1\n  y: 2\n}\nb {\n  $a\n  x: 3\n  z: 4\n}", "b") shouldBe "x: 3\ny: 2\nz: 4"
    // what stands before it, it overrides
    shown("a { x: 1, y: 2 }\nb { x: 3, $a }", "b") shouldBe "x: 1\ny: 2"
    // in a block opened by indentation too
    shown("a { x: 1, y: 2 }\nb:\n  $a\n  x: 3", "b") shouldBe "x: 3\ny: 2"
    shown("a { x: 1, y: 2 }\nb:\n  x: 3\n  $a", "b") shouldBe "x: 1\ny: 2"
  }

  it should "be written with braces or without" in {
    for (injection <- List("$a", "${a}"))
      shown("a { x: 1 }\nb { " + injection + ", y: 2 }", "b") shouldBe "x: 1\ny: 2"
  }

  it should "keep where each field came from, its own tags and none of the map's" in {
    loaded("a: @t { x: @u 1 }\nb { $a }")("b") match {
      case map: Node.IMap[?] =>
        map.tag shouldBe None
        map("x") shouldBe
          Node.IScalar(Id.Key("x", Id.Key("b", Id.Root)), Some("u"), "1", ReferenceLocation(at(2, 5), "a.x", at(1, 15)))
      case other => fail(other.toString)
    }
  }

  it should "merge deep, as ':' does, and replace where ':=' does" in {
    val a = "a { c { p: 1, q: 2 } }\n"
    shown(a + "b { $a, c { q: 3 } }", "b") shouldBe "c:\n  p: 1\n  q: 3"
    shown(a + "b { $a, c := { q: 3 } }", "b") shouldBe "c:\n  q: 3"
    // a block written before it merges with the one it brings in
    shown(a + "b { c { r: 0 }, $a }", "b") shouldBe "c:\n  r: 0\n  p: 1\n  q: 2"
    // a value that is no map replaces one, and the other way round
    shown(a + "b { c: 5, $a }", "b") shouldBe "c:\n  p: 1\n  q: 2"
  }

  it should "take the map as it ends up, merged from every source, and what is merged after the injection too" in {
    shown("a { x: 1 }\nb { $a }\na.y: 2\nb { z: 3 }\nb.x: 4", "b") shouldBe "x: 4\ny: 2\nz: 3"
  }

  it should "inject a map that holds injections of its own, and one into a block that holds its own" in {
    shown("a { x: 1 }\nm { $a, y: 2 }\nn { $m, z: 3 }", "n") shouldBe "x: 1\ny: 2\nz: 3"
    // `c` has an injection of its own when `$a` brings a `c` in, which is merged after it
    shown("a { c { u: 1 } }\nd { c { v: 2 } }\nb { c { $d.c }, $a }", "b") shouldBe
      "c:\n  v: 2\n  u: 1"
  }

  it should "not stand at the root, which every path goes through, nor refer to the block it stands in" in {
    errors("a { x: 1 }\n$a\nb: 2\n$?c") shouldBe List(InjectionAtRoot(at(2, 1)), InjectionAtRoot(at(4, 1)))
    errors("m { a { x: 1 }, $m.a }") shouldBe List(SubstitutionCycle(at(1, 17), "m.a"))
  }

  it should "stand in the block a source with one at its top is included in" in {
    val file = java.nio.file.Files.createTempFile("injection", ".psi")
    try {
      java.nio.file.Files.write(file, "$a\ny: 2".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      shown("a { x: 1 }\nb { #include(url: \"" + file.toUri.toString + "\") }", "b") shouldBe "x: 1\ny: 2"
    } finally java.nio.file.Files.delete(file)
  }

  it should "be looked into by a path, and find nothing when optional and missing" in {
    value(loaded("a { x: 1 }\nb { $a }\nc: $b.x")("c")) shouldBe "1"
    // a field may refer to another of the same block, the injection merged before either is resolved
    shown("a { x: 1 }\nm { $a, y: $m.x }", "m") shouldBe "x: 1\ny: 1"
    shown("b { $?nope, y: 1 }", "b") shouldBe "y: 1"
  }

  it should "take no tags, which would have no node to go to" in {
    errors("a { x: 1 }\nb:\n  @t $a\n  y: 2") shouldBe List(TaggedInjection(at(3, 3)))
    errors("a { x: 1 }\nb:\n  y: 2\n  @t $a") shouldBe List(TaggedInjection(at(4, 3)))
    // in braces a tag is no key
    errors("a { x: 1 }\nb { @t $a }") shouldBe List(TagBeforeKey(at(2, 5)))
  }

  "A substitution in brackets" should "put the elements of a sequence it finds there, and anything else whole" in {
    shown("a: [x, y]\nb: [a, $a, z]", "b") shouldBe "- a\n- x\n- y\n- z"
    shown("a: [x, y]\nb: [a, ${a}, z]", "b") shouldBe "- a\n- x\n- y\n- z"
    shown("a: []\nb: [a, $a, z]", "b") shouldBe "- a\n- z"
    shown("a: x\nb: [a, $a, z]", "b") shouldBe "- a\n- x\n- z"
    shown("a { x: 1 }\nb: [$a]", "b") shouldBe "- x: 1"
    shown("a: @u x\nb: [@t $a]", "b") shouldBe "- @t x"
    shown("b: [a, $?nope, ${?nope}, z]", "b") shouldBe "- a\n- z"
    // brackets around it nest a sequence whole
    shown("a: [x, y]\nb: [a, [$a], z]", "b") shouldBe "- a\n- - x\n  - y\n- z"
  }

  it should "take no tags before a sequence, whose elements keep their own" in {
    errors("a: [1]\nb: [@t $a]") shouldBe List(TaggedSequence(at(2, 8), "a", at(1, 4)))
  }

  "A substitution alone on its line among '- ' elements" should "put the elements of a sequence there" in {
    shown("a: [x, y]\nb:\n  - a\n  $a\n  - z", "b") shouldBe "- a\n- x\n- y\n- z"
    // before the first `- ` too, and optional
    shown("a: [x, y]\nb:\n  $a\n  ${?nope}\n  - z", "b") shouldBe "- x\n- y\n- z"
  }

  it should "be an error when it finds no sequence, or has a tag" in {
    errors("a: x\nb:\n  - p\n  $a") shouldBe List(InjectionNotSequence(at(4, 3), "a", at(1, 4)))
    errors("a { x: 1 }\nb:\n  - p\n  $a") shouldBe List(InjectionNotSequence(at(4, 3), "a", at(1, 3)))
    errors("a: [1]\nb:\n  - p\n  @t $a") shouldBe List(TaggedInjection(at(4, 3)))
    errors("a: [1]\nb:\n  @t $a\n  - p") shouldBe List(TaggedInjection(at(3, 3)))
    // anything else alone on its line is no element
    errors("b:\n  - p\n  q") shouldBe List(MixedBlock(at(3, 3)))
  }

  "A substitution after a '- '" should "stand there as one element" in {
    shown("a: [x, y]\nb:\n  - a\n  - $a\n  - z", "b") shouldBe "- a\n- - x\n  - y\n- z"
    shown("a: [x, y]\nb:\n  -\n    $a", "b") shouldBe "- - x\n  - y"
  }

  "A substitution alone after a key, on its line or below it" should "be the key's value, whole" in {
    loaded("a: @u x\nb:\n  @t $a")("b") shouldBe Node.IScalar(Id.Key("b", Id.Root), Some("t"), "x",
      ReferenceLocation(at(3, 6), "a", at(1, 7)))
    shown("a: [x]\nb:\n  $a", "b") shouldBe "- x"
    shown("a: @u { x: 1 }\nb:\n  $a", "b") shouldBe "@u\n  x: 1"
  }

  "Substitutions alone in a block, two or more" should "merge the maps they find, as injections among fields" in {
    shown("a { x: 1, y: 2 }\nc { y: 3 }\nb:\n  $a\n  $c", "b") shouldBe "x: 1\ny: 3"
    shown("a { x: 1 }\nb: @t\n  $?nope\n  $a\n  ${?nope}", "b") shouldBe "@t\n  x: 1"
    shown("a { x: 1 }\nc { y: 2 }\nb:\n  - p\n  -\n    $a\n    $c", "b") shouldBe "- p\n- x: 1\n  y: 2"
  }

  it should "put the elements of the sequences they find, as injections among elements" in {
    shown("a: [x, y]\nc: [z]\nb:\n  $a\n  $c", "b") shouldBe "- x\n- y\n- z"
    shown("a: [x, y]\nb:\n  $?nope\n  $a\n  $a", "b") shouldBe "- x\n- y\n- x\n- y"
  }

  it should "put nothing when none finds anything" in {
    loaded("b:\n  $?nope\n  $?none\nc: 1")("b") shouldBe Node.INone(Id.Key("b", Id.Root))
    shown("b:\n  - p\n  -\n    $?nope\n    $?none\n  - q", "b") shouldBe "- p\n- q"
    value(loaded("b:\n  - p\n  -\n    $?nope\n    $?none\n  - q\nc: $b[1]")("c")) shouldBe "q"
    // after a `- `, an optional substitution that finds nothing puts no element either
    val optional = "x: 1\nb:\n  - $?nope\n  - $?x\n  - q\n"
    value(loaded(optional + "c: $b[0]")("c")) shouldBe "1"
    value(loaded(optional + "c: $b[1]")("c")) shouldBe "q"
    errors("b:\n  - $?b\n  - q\nc: $b[1]") shouldBe List(SubstitutionCycle(at(2, 5), "b"))
  }

  it should "be what the first that finds anything finds, a map or a sequence, the others alike" in {
    errors("a { x: 1 }\nc: [1]\nb:\n  $a\n  $c") shouldBe List(InjectionNotMap(at(5, 3), "c", at(2, 4)))
    errors("a { x: 1 }\nc: [1]\nb:\n  $c\n  $a") shouldBe List(InjectionNotSequence(at(5, 3), "a", at(1, 3)))
    errors("a: x\nc: [1]\nb:\n  $a\n  $c") shouldBe List(InjectionNotContainer(at(4, 3), "a", at(1, 4)))
    // past the first that does not fit nothing more is merged
    errors("a { x: 1 }\nc: [1]\nb:\n  $a\n  $c\n  $a") shouldBe List(InjectionNotMap(at(5, 3), "c", at(2, 4)))
    errors("b:\n  $nope\n  $none") shouldBe
      List(UnresolvedSubstitution(at(2, 3), "nope"), UnresolvedSubstitution(at(3, 3), "none"))
  }

  it should "be looked into by a path, and merge with what is written elsewhere at its key once resolved" in {
    value(loaded("a { x: 1 }\nc { y: 2 }\nb:\n  $a\n  $c\nd: $b.y")("d")) shouldBe "2"
    value(loaded("a: [x]\nc: [y]\nb:\n  $a\n  $c\nd: $b[1]")("d")) shouldBe "y"
    shown("a { x: 1 }\nb:\n  $a\n  $a\nb.y: 2", "b") shouldBe "x: 1\ny: 2"
    shown("a { x: 1 }\nb { y: 2 }\nb:\n  $a\n  $a", "b") shouldBe "y: 2\nx: 1"
    shown("a { x: 1 }\nb: $a\nb:\n  $a\n  $a", "b") shouldBe "x: 1"
    // an injection brings in a map over one
    shown("a { c { p: 1 } }\nx { q: 2 }\nb {\n  c:\n    $x\n    $x\n  $a\n}", "b") shouldBe "c:\n  q: 2\n  p: 1"
    // nor refer to itself, each reference closing a cycle
    errors("b:\n  $b\n  $b") shouldBe List(SubstitutionCycle(at(2, 3), "b"), SubstitutionCycle(at(3, 3), "b"))
  }

  "An index into a sequence" should "count what its injections put, and resolve only what stands before" in {
    val text = "a: [x, y]\nb: [p, $a, $?nope, q, $c]\nc: r\n"
    value(loaded(text + "d: $b[2]")("d")) shouldBe "y"
    value(loaded(text + "d: $b[3]")("d")) shouldBe "q"
    value(loaded(text + "d: $b[4]")("d")) shouldBe "r"
    errors(text + "d: $b[5]") shouldBe List(UnresolvedSubstitution(at(4, 4), "b[5]"))
    errors("a: x\nb:\n  $a\n  - q\nd: $b[0]") shouldBe List(InjectionNotSequence(at(3, 3), "a", at(1, 4)))
    // an element refers to one before it in its own sequence
    shown("a: [x, $a[0]]", "a") shouldBe "- x\n- x"
    shown("a: [[x, y], $a[0]]", "a") shouldBe "- - x\n  - y\n- x\n- y"
    shown("a: [x, $b, $a[0]]\nb: [y]", "a") shouldBe "- x\n- y\n- x"
  }

  "A '*' after a substitution" should "be text, as it is after anything else" in {
    value(loaded("a: x\nb: $a*")("b")) shouldBe "x*"
    value(loaded("a: x\nb: ${a}*")("b")) shouldBe "x*"
    value(loaded("b: \"$a*\"")("b")) shouldBe "$a*"
    errors("a { x: 1 }\nb { $a* }") shouldBe List(MisplacedInjection(at(2, 5)))
    errors("a: x\nb: ${a*}") shouldBe List(InvalidSubstitution(at(2, 4)))
  }

  "A directive in brackets or among '- ' elements" should "put the elements of a sequence it gives there" in {
    val seq = Psi.parse("d", "s: [p, @u q]").fold(e => fail(e.toString), _("s")) match {
      case seq: Node.ISeq[?] => seq
      case other => fail(other.toString)
    }
    val scalar = Psi.parse("d", "s: v").fold(e => fail(e.toString), _("s")) match {
      case scalar: Node.IScalar[?] => scalar
      case other => fail(other.toString)
    }
    val map = Psi.parse("d", "s { m: 1 }").fold(e => fail(e.toString), _("s")) match {
      case map: Node.IMap[?] => map
      case other => fail(other.toString)
    }
    def giving(called: String, node: Option[Node.ISome[Id]]): (String, Directive) =
      called -> new Directive {
        def parameters: Seq[Parameter] = Nil
        def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
          Right(node.fold[Fragment](Fragment.None)(Fragment(_)))
      }
    def parsing(called: String, text: String): (String, Directive) =
      called -> new Directive {
        def parameters: Seq[Parameter] = Nil
        def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
          Right(context.parse("p", text))
      }
    val by = Psi.builder
      .withDirectives(
        giving("seq", Some(seq)),
        giving("one", Some(scalar)),
        giving("map", Some(map)),
        giving("none", None),
        parsing("parsed", "x: 1"),
        parsing("blocks", "c:\n  #map\n  $?nope")
      )
      .build
      .fold(e => fail(e.toString), identity)
    def loaded(text: String): Node.IMap[Id.Root] = by.parse("s", text).fold(e => fail(e.toString), identity)
    def shown(text: String): String = loaded(text)("b").toString
    // what it puts stands where it does, under the depth limit
    val nested = Psi.parse("d", "s: [[p]]").fold(e => fail(e.toString), _("s")) match {
      case found: Node.ISeq[?] => found
      case other => fail(other.toString)
    }
    def deep(text: String): List[PsiError] =
      Psi.builder.withDirectives(giving("seq", Some(nested))).withDepthLimit(1).build
        .fold(e => fail(e.toString), identity)
        .parse("s", text)
        .fold(_.errors, root => fail(root.toString))
    deep("b: [#seq]") shouldBe List(DepthExceeded(at(1, 5), 1, fatal = false))
    deep("b:\n  #seq\n  #seq") shouldBe List(DepthExceeded(at(2, 3), 1, fatal = false),
      DepthExceeded(at(3, 3), 1,
        fatal = false))
    shown("b: [a, #seq, z]") shouldBe "- a\n- p\n- @u q\n- z"
    shown("b: [a, #seq(), z]") shouldBe "- a\n- p\n- @u q\n- z"
    // in brackets anything else stands as one element, as a field's value
    shown("b: [a, @t #one, z]") shouldBe "- a\n- @t v\n- z"
    shown("b: [#parsed]") shouldBe "- x: 1"
    shown("b: [a, #none, z]") shouldBe "- a\n- z"
    shown("b: #seq") shouldBe "- p\n- @u q"
    // alone on its line among `- ` elements, a sequence's; after a `- `, one element
    shown("b:\n  - a\n  #seq\n  - #seq\n  -\n    #seq") shouldBe "- a\n- p\n- @u q\n- - p\n  - @u q\n- - p\n  - @u q"
    // alone in a block, two or more, what the first that gives anything gives decides
    shown("b:\n  #none\n  #seq\n  #seq") shouldBe "- p\n- @u q\n- p\n- @u q"
    shown("a: [r]\nb:\n  #seq\n  $a") shouldBe "- p\n- @u q\n- r"
    shown("a { y: 2 }\nb:\n  #parsed\n  $a\n  #map") shouldBe "x: 1\ny: 2\nm: 1"
    shown("a { y: 2 }\nb:\n  #none\n  $a\n  $a") shouldBe "y: 2"
    // a call that gives nothing puts nothing, alone on its line among `- ` elements or among injections alone
    shown("b:\n  - p\n  #none") shouldBe "- p"
    shown("b:\n  #parsed\n  #none") shouldBe "x: 1"
    loaded("b:\n  #none\n  #none\nc: 1")("b") shouldBe Node.INone(Id.Key("b", Id.Root))
    def errors(text: String): List[PsiError] = by.parse("s", text).fold(_.errors, root => fail(root.toString))
    errors("b: [@t #seq]") shouldBe List(TaggedDirectiveSequence(at(1, 8), "seq"))
    errors("b:\n  - p\n  #one") shouldBe List(DirectiveNotSequence(at(3, 3), "one"))
    errors("b:\n  - p\n  #parsed") shouldBe List(DirectiveNotSequence(at(3, 3), "parsed"))
    errors("b:\n  - p\n  @t #seq") shouldBe List(TaggedInjection(at(3, 3)))
    errors("b:\n  #seq\n  #map") shouldBe List(DirectiveNotSequence(at(3, 3), "map"))
    errors("b:\n  #map\n  #seq") shouldBe List(DirectiveNotMap(at(3, 3), "seq"))
    // a substitution and a call decide alike, the first as written that finds or gives anything
    shown("a: [r]\nb:\n  $?nope\n  $a\n  #seq") shouldBe "- r\n- p\n- @u q"
    errors("a: [r]\nb:\n  $a\n  #map") shouldBe List(DirectiveNotSequence(at(4, 3), "map"))
    errors("a { y: 2 }\nb:\n  $a\n  #seq") shouldBe List(DirectiveNotMap(at(4, 3), "seq"))
    errors("a: x\nb:\n  $a\n  #map") shouldBe List(InjectionNotContainer(at(3, 3), "a", at(1, 4)))
    // what a call gave in a source it parsed waits there too
    shown("b: #blocks") shouldBe "c:\n  m: 1"
    errors("b:\n  #none\n  #one\n  #seq") shouldBe List(DirectiveNotContainer(at(3, 3), "one"))
    errors("a: [1]\nb:\n  #map\n  $a") shouldBe List(InjectionNotMap(at(4, 3), "a", at(1, 4)))
    // a `*` after a call is a second value
    errors("b: [#seq*]") shouldBe List(MissingSeparator(at(1, 9), "'*'"))
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
      List(UnresolvedSubstitution(at(1, 5), "missing"), UnresolvedSubstitution(at(1, 18), "a"))
    // each group at the same keys, apart: what one resolved is not taken for another's
    errors("a { $first, x: $one, $second, x: $two }") shouldBe
      List(
        UnresolvedSubstitution(at(1, 5), "first"),
        UnresolvedSubstitution(at(1, 16), "one"),
        UnresolvedSubstitution(at(1, 22), "second"),
        UnresolvedSubstitution(at(1, 34), "two")
      )
    errors("a { $first, x { y: $one }, $second, x { y: $two } }") shouldBe
      List(
        UnresolvedSubstitution(at(1, 5), "first"),
        UnresolvedSubstitution(at(1, 20), "one"),
        UnresolvedSubstitution(at(1, 28), "second"),
        UnresolvedSubstitution(at(1, 44), "two")
      )
    // what a group resolved elsewhere is kept, and counted once: `o`, resolved there first, copies 3 nodes, and each `x`
    // 3 more
    val counted = "a { $first, x: $o, $second, x: $o }\np: [1, 2]\no: $p"
    errors(counted, loader(nodes = 9)) shouldBe
      List(UnresolvedSubstitution(at(1, 5), "first"), UnresolvedSubstitution(at(1, 20), "second"))
    errors(counted, loader(nodes = 8)) shouldBe
      List(
        UnresolvedSubstitution(at(1, 5), "first"),
        UnresolvedSubstitution(at(1, 20), "second"),
        ResolutionTooLarge(at(1, 32), "o", 8))
    // and so is it from a group within a group
    errors("a { $first, x { $f, y: $o, $s, y: $o }, $second }\np: [1]\no: $p", loader(nodes = 6)) shouldBe
      List(
        UnresolvedSubstitution(at(1, 5), "first"),
        UnresolvedSubstitution(at(1, 17), "f"),
        UnresolvedSubstitution(at(1, 28), "s"),
        UnresolvedSubstitution(at(1, 41), "second")
      )
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
    // a quoted one, which is no key, is written back as it was
    errors("\"$" + "{a}$" + "{b}\": 1") shouldBe List(QuotedKey(at(1, 1), "$" + "{a}$" + "{b}"))
  }

  "What an injection brings in" should "merge with a substitution once it is resolved" in {
    shown("a { c { p: 1 } }\nx { q: 2 }\nb { c: $x, $a }", "b") shouldBe "c:\n  q: 2\n  p: 1"
    // what is written after the injection merges as it is written
    shown("a { c { p: 1 } }\nx { q: 2 }\nb { $a, c: $x, c { r: 3 } }", "b") shouldBe "c:\n  p: 1\n  q: 2\n  r: 3"
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
    errors("a: [1, 2]\nb:\n  $a\n  $a", loader(nodes = 2)) shouldBe List(ResolutionTooLarge(at(4, 3), "a", 2))
  }

  "A tree with injections" should "be written back as it is written" in {
    def merged(text: String): String = {
      val parsed = Parser.parse(Source("s", text))
      parsed.errors shouldBe Nil
      Render.document(Merge(::((parsed.value, at(1, 1)), Nil)).value)
    }
    merged("a: [1]\nb: [$a, @t ${?c}, { x: $?d }, [$e]]") shouldBe
      "a:\n  - 1\nb:\n  [\n    ${a}\n    @t ${?c}\n    {\n      x: ${?d}\n    }\n    [\n      ${e}\n    ]\n  ]\n"
    merged("b:\n  - [$a]\n  - 1\n  $c") shouldBe "b:\n  - [\n      ${a}\n    ]\n  - 1\n  ${c}\n"
    merged("b:\n  $a\n  $?c\nd:\n  -\n    $a\n    $c") shouldBe
      "b:\n  ${a}\n  ${?c}\nd:\n  - ${a}\n    ${c}\n"
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
        "a tag before an injection, whose fields or elements keep their own tags",
        Some("remove the tag, or write '- ' before it to put the node itself as one element")))
    val tagged = TaggedSequence(at(1, 1), "a", at(2, 3))
    (tagged.message, tagged.hint) shouldBe
      ((
        "a tag before '$" + "{a}', a sequence whose elements stand in its place, keeping their own tags; it is " +
          "defined at s:2:3",
        Some("remove the tag, or nest the sequence whole: [$" + "{a}]")))
    val call = TaggedDirectiveSequence(at(1, 1), "d")
    (call.message, call.hint) shouldBe
      ((
        "a tag before '#d', which gave a sequence whose elements stand in its place, keeping their own tags",
        Some("remove the tag, or nest the sequence whole: [#…]")))
    (InjectionNotSequence(at(1, 1), "a", at(2, 3)).message, InjectionNotSequence(at(1, 1), "a", at(2, 3)).hint) shouldBe
      ((
        "'$" + "{a}' injected among elements is not a sequence; it is defined at s:2:3",
        Some("write '- ' before it to put it as one element")))
    (DirectiveNotSequence(at(1, 1), "d").message, DirectiveNotSequence(at(1, 1), "d").hint) shouldBe
      ((
        "'#d' injected among elements gave something other than a sequence",
        Some("write '- ' before it to put what it gives as one element")))
    val neither = InjectionNotContainer(at(1, 1), "a", at(2, 3))
    (neither.message, neither.hint) shouldBe
      ((
        "'$" + "{a}' injected in a block of injections alone is neither a map nor a sequence; it is defined at s:2:3",
        Some("give the block one value alone to take it whole, or write the values as elements with '- '")))
    (DirectiveNotContainer(at(1, 1), "d").message, DirectiveNotContainer(at(1, 1), "d").hint) shouldBe
      ((
        "'#d' in a block of injections alone gave neither a map nor a sequence",
        Some("give the block one value alone to take it whole, or write the values as elements with '- '")))
  }
}
