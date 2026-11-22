package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResolveTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def ref(line: Int, column: Int, path: String, origin: PsiLocation): ReferenceLocation =
    ReferenceLocation(at(line, column), path, origin)

  private def load(text: String, properties: Map[String, String]): Either[PsiErrors, Node.IMap[Id.Root]] =
    Psi.load(Right(Source("s", text)) :: Nil, properties)

  private def loaded(text: String, properties: Map[String, String] = Map.empty): Node.IMap[Id.Root] =
    load(text, properties).fold(errors => fail(errors.toString), identity)

  private def errors(text: String, properties: Map[String, String] = Map.empty): List[PsiError] =
    load(text, properties).fold(_.errors, root => fail(root.toString))

  private def keyed(names: String*): Id.Key =
    names.tail.foldLeft(Id.Key(names.head, Id.Root))((parent, name) => Id.Key(name, parent))

  "A substitution" should "graft a scalar at its own position, located where it was taken from" in {
    loaded("a: ${b}\nb: 1")("a") shouldBe Node.IScalar(keyed("a"), None, "1", ref(1, 4, "b", at(2, 4)))
  }

  it should "graft a map whole, every node of it at its new position, the tag written in place of its own" in {
    val root = loaded("a: @y ${b}\nb: @x { c: 1, d: [2] }")
    root("a") match {
      case map: Node.IMap[?] =>
        map.id shouldBe keyed("a")
        map.tag shouldBe Some("y")
        map.location shouldBe ref(1, 7, "b", at(2, 7))
        map("c") shouldBe Node.IScalar(keyed("a", "c"), None, "1", ref(1, 7, "b.c", at(2, 12)))
        map("d") match {
          case seq: Node.ISeq[?] =>
            seq.location shouldBe ref(1, 7, "b.d", at(2, 18))
            seq(0) shouldBe Node.IScalar(Id.Index(0, keyed("a", "d")), None, "2", ref(1, 7, "b.d[0]", at(2, 19)))
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
    root("b") match {
      case map: Node.IMap[?] => map.tag shouldBe Some("x")
      case other => fail(other.toString)
    }
    loaded("a: ${b}\nb: @x { c: 1 }")("a") match {
      case map: Node.IMap[?] => map.tag shouldBe Some("x")
      case other => fail(other.toString)
    }
  }

  it should "chain the references of a chain of substitutions" in {
    loaded("a: ${b}\nb: ${c}\nc: 1")("a") shouldBe
      Node.IScalar(keyed("a"), None, "1", ref(1, 4, "b", ref(2, 4, "c", at(3, 4))))
  }

  it should "graft null as null" in {
    loaded("a: ${n}\nn: null")("a") shouldBe Node.INull(keyed("a"), None, ref(1, 4, "n", at(2, 4)))
  }

  it should "look into a sequence and below a substitution" in {
    val root = loaded("s: [0, { k: v }]\nt: ${s[1].k}\nu: ${w.k}\nw: ${s[1]}")
    root("t") shouldBe Node.IScalar(keyed("t"), None, "v", ref(2, 4, "s[1].k", at(1, 13)))
    root("u") shouldBe Node.IScalar(keyed("u"), None, "v", ref(3, 4, "w.k", ref(4, 4, "s[1].k", at(1, 13))))
  }

  "An optional substitution" should "drop its field when nothing is there, and graft what is" in {
    val root = loaded("a: ${?x}\nb: ${?c}\nc: 1")
    root("a") shouldBe Node.None(keyed("a"))
    root.size shouldBe 2
    root("b") shouldBe Node.IScalar(keyed("b"), None, "1", ref(2, 4, "c", at(3, 4)))
  }

  it should "leave nothing to look into" in {
    errors("o: ${?none}\np: ${o.k}") shouldBe List(UnresolvedSubstitution(at(2, 4), "o.k"))
  }

  it should "drop its element in a sequence, the elements after it moving up" in {
    val root = loaded("s: [${?x}, 1, ${?y}, 2]\nt: ${s[1]}\nu: ${?s[2]}")
    root("s") match {
      case seq: Node.ISeq[?] => seq.toList.collect { case s: Node.IScalar[?] => s.value } shouldBe List("1", "2")
      case other => fail(other.toString)
    }
    root("t") shouldBe Node.IScalar(keyed("t"), None, "2", ref(2, 4, "s[1]", at(1, 22)))
    root("u") shouldBe Node.None(keyed("u"))
  }

  it should "count in a sequence when it grafts something" in {
    val root = loaded("s: [${?x}, 1]\nt: ${s[1]}\nu: ${s[0]}\nx: 0")
    root("t") shouldBe Node.IScalar(keyed("t"), None, "1", ref(2, 4, "s[1]", at(1, 12)))
    root("u") shouldBe Node.IScalar(keyed("u"), None, "0", ref(3, 4, "s[0]", ref(1, 5, "x", at(4, 4))))
  }

  it should "find each element of a sequence by index, however many lookups go into it, in any order" in {
    def values(text: String, keys: String*): Seq[String] = {
      val root = loaded(text)
      keys.map(key =>
        root(key) match {
          case scalar: Node.IScalar[?] => scalar.value
          case other => other.toString
        })
    }
    val lookups = "\nk3: $s[3]\nk0: $s[0]\nk5: $?s[5]\nk2: $s[2]\nk4: $s[4]\nk1: $s[1]"
    values("t: [a, b]\ns: [x, $t*, ${?n}, ${t; join(\"-\")}, y]" + lookups, "k0", "k1", "k2", "k3", "k4", "k5") shouldBe
      Seq("x", "a", "b", "a-b", "y", "INone(Key(k5,Root))")
    // an injection after a key gives its sequence anew to each lookup
    values("t: [a, b]\ns: $t*\nk2: $?s[2]\nk1: $s[1]\nk0: $s[0]", "k0", "k1", "k2") shouldBe
      Seq("a", "b", "INone(Key(k2,Root))")
  }

  it should "count from the end with an index after a '-', -1 the last element, walking past every injection" in {
    def values(text: String, keys: String*): Seq[String] = {
      val root = loaded(text)
      keys.map(key =>
        root(key) match {
          case scalar: Node.IScalar[?] => scalar.value
          case other => other.toString
        })
    }
    val lookups = "\nm2: $s[-2]\nm5: $?s[-5]\nm1: $s[-1]\nk1: $s[1]\nm4: $s[-4]\nm3: $s[-3]"
    values("t: [a, b]\ns: [x, $t*, ${?n}, y]" + lookups, "m1", "m2", "m3", "m4", "m5", "k1") shouldBe
      Seq("y", "b", "a", "x", "INone(Key(m5,Root))", "a")
    values("s: [x]\ns: [y, z]\nl: $s[-1]\ne: []\nf: $?e[-1]", "l", "f") shouldBe Seq("z", "INone(Key(f,Root))")
  }

  it should "take the last of a list gathered from several, located where it was written" in {
    loaded("a.hosts: [h1, h2]\nb.hosts: [h3]\nall: [$a.hosts*, $b.hosts*]\nlast: $all[-1]")("last") shouldBe
      Node.IScalar(keyed("last"), None, "h3", ref(4, 7, "all[-1]", ref(3, 18, "b.hosts[0]", at(2, 11))))
  }

  it should "find nothing before the first element, as past the last" in {
    errors("s: [1, 2]\nt: $s[-3]") shouldBe List(UnresolvedSubstitution(at(2, 4), "s[-3]"))
    // an element after it failing, the end is not known, and only that element's error is reported
    errors("s: [1, $nope]\nt: $s[-2]") shouldBe List(UnresolvedSubstitution(at(1, 8), "nope"))
  }

  it should "read as empty in a concatenation" in {
    loaded("e: \"${?x}\"")("e") shouldBe Node.IScalar(keyed("e"), None, "", at(1, 4))
  }

  "A concatenation" should "join scalars, with a reference per substitution and only its own tags" in {
    loaded("h: @t db\np: 5432\nurl: @u \"jdbc:${h}:${p}/x${?none}\"")("url") shouldBe
      Node.IScalar(
        keyed("url"),
        Some("u"),
        "jdbc:db:5432/x",
        ConcatenationLocation(at(3, 9), ::(ref(3, 15, "h", at(1, 7)), ref(3, 20, "p", at(2, 4)) :: Nil)))
  }

  it should "be resolved once, however many refer to it" in {
    val root = loaded("c: \"x${d}\"\nd: 1\ne: ${c}\nf: ${c}")
    List(root("e"), root("f")).collect { case s: Node.IScalar[?] => s.value } shouldBe List("x1", "x1")
  }

  it should "refuse a node that is not a scalar" in {
    errors("m {}\nn: null\nc: \"${m}${n}\"") shouldBe
      List(NonScalarInConcatenation(at(3, 5), "m", at(1, 3)), NonScalarInConcatenation(at(3, 9), "n", at(2, 4)))
  }

  "A missing node" should "be an error, at every reference that names it" in {
    errors("a: ${x}\nb: \"${y.z}\"\nc: [${s[5]}]\ns: [1]\nq: \"x${?none}\"\nd: ${q.k}") shouldBe
      List(
        UnresolvedSubstitution(at(1, 4), "x"),
        UnresolvedSubstitution(at(2, 5), "y.z"),
        UnresolvedSubstitution(at(3, 5), "s[5]"),
        UnresolvedSubstitution(at(6, 4), "q.k")
      )
  }

  it should "be reported as the root cause only, what depends on it dropped silently" in {
    errors("a: ${x}\nb: ${a}\nc: \"${a}\"\nd: [${a}]\ne { f: ${a} }\ng: ${e}") shouldBe
      List(UnresolvedSubstitution(at(1, 4), "x"))
  }

  "A key merged with a substitution" should "report what the substitution does not find, and fail what refers to it" in {
    errors("a {}\na: ${target}\nb: ${a}\nc: ${m.k}\nm { k: ${t}, k {} }\nn: ${m}\nd: ${x}\ntarget: 1",
      Map("a" -> "1", "m.k" -> "1")) shouldBe
      List(UnresolvedSubstitution(at(5, 8), "t"), UnresolvedSubstitution(at(7, 4), "x"))
  }

  it should "merge a map over what it finds, maps merging and anything else replaced, either way" in {
    def shown(text: String): String = Psi.parse("s", text).fold(e => fail(e.toString), _("a").toString)
    shown("a: $b\na.x: 1\nb { y: 2 }") shouldBe "y: 2\nx: 1"
    // as `:` merges two maps, the later tag if written, the earlier otherwise
    shown("b: @t { y: 2 }\na: $b\na.x: 1") shouldBe "@t\n  y: 2\n  x: 1"
    shown("b: @t { y: 2 }\na: $b\na: @u { x: 1 }") shouldBe "@u\n  y: 2\n  x: 1"
    shown("b: @t { y: 2 }\na: @u { x: 1 }\na: $b") shouldBe "@t\n  x: 1\n  y: 2"
    shown("b { y: 2 }\na: @u { x: 1 }\na: $b") shouldBe "@u\n  x: 1\n  y: 2"
    shown("b: @t { y: 2 }\na: @u $b\na.x: 1") shouldBe "@u\n  y: 2\n  x: 1"
    shown("b: 5\na: $b\na.x: 1") shouldBe "x: 1"
    shown("a { x: 1 }\na: $b\nb { y: 2 }") shouldBe "x: 1\ny: 2"
    Psi.parse("s", "a { x: 1 }\na: $b\nb: 5").fold(e => fail(e.toString), _("a")) match {
      case h8io.cfg.Node.IScalar(_, _, value, _) => value shouldBe "5"
      case other => fail(other.toString)
    }
    shown("a { x: 1 }\na: $b\nb: [5]\na.y: 2") shouldBe "y: 2"
    // what finds nothing puts nothing
    shown("a: $?b\na.x: 1") shouldBe "x: 1"
    Psi.parse("s", "a: $?b\na: $?c\nd: 1").fold(e => fail(e.toString), _("a")) shouldBe
      h8io.cfg.Node.INone(h8io.cfg.Id.Key("a", h8io.cfg.Id.Root))
    // a sequence replaces, and an injection after a key, or a block of them, merges as what it turns out
    shown("a: $s\na: $t\ns: [1]\nt: [2]") shouldBe "- 2"
    shown("s: [1]\nm { k: 1 }\na: $s*\na: $m") shouldBe "k: 1"
    shown("s: [1]\nm { k: 1 }\na: $m\na: $s*") shouldBe "- 1"
    shown("m { k: 1 }\nn { j: 2 }\na: $m\na: $n*") shouldBe "k: 1\nj: 2"
    shown("m { k: 1 }\nn { j: 2 }\na: $m\na:\n  $n*\n  $n*") shouldBe "k: 1\nj: 2"
    // looked into by a path, and at any depth, layers within layers
    Psi.parse("s", "a: $b\na.x: 1\nb { y: 2 }\nc: $a.y").fold(e => fail(e.toString), _("c")) match {
      case h8io.cfg.Node.IScalar(_, _, value, _) => value shouldBe "2"
      case other => fail(other.toString)
    }
    shown("x { s: 1 }\na { k { p: 1 } }\na: $b\nb {}\na { k: $x, k { q: 2 } }") shouldBe
      "k:\n  p: 1\n  s: 1\n  q: 2"
  }

  it should "merge a block over what it finds as written there, a `:=` in it replacing" in {
    def shown(text: String): String = Psi.parse("s", text).fold(e => fail(e.toString), _("a").toString)
    shown("b { n { old: 1 } }\na: $b\na { n := { fresh: 2 } }") shouldBe "n:\n  fresh: 2"
    shown("b { n { old: 1 } }\na: $b\na.n := { fresh: 2 }") shouldBe "n:\n  fresh: 2"
    shown("b: @t { n { old: 1 } }\na: $b\na: @u { n := { fresh: 2 } }") shouldBe "@u\n  n:\n    fresh: 2"
    shown("a: $?b\na { n := { fresh: 2 } }") shouldBe "n:\n  fresh: 2"
    // an injection in the block merges where it is written, before the `:=` or after it
    shown("b { n { old: 1 }, k: 1 }\nc { n { c: 1 } }\na: $b\na { $c*, n := { fresh: 2 } }") shouldBe
      "n:\n  fresh: 2\nk: 1"
    shown("b { n { old: 1 }, k: 1 }\nc { n { c: 1 } }\na: $b\na { n := { fresh: 2 }, $c* }") shouldBe
      "n:\n  fresh: 2\n  c: 1\nk: 1"
    // over a block of injections alone, and with more merged after it
    shown("b { n { old: 1 } }\nx { k: 1 }\na:\n  $b*\n  $x*\na { n := { fresh: 2 } }") shouldBe
      "n:\n  fresh: 2\nk: 1"
    shown("b { n { old: 1 } }\na: $b\na { n := { fresh: 2 } }\na { n { more: 3 } }") shouldBe
      "n:\n  fresh: 2\n  more: 3"
    // what is written replaces in turn, as it would where the substitution stands
    shown("b { n { old: 1 } }\na: $b\na { n: 1, n { new: 2 } }") shouldBe "n:\n  new: 2"
    errors("b { n { old: 1 } }\na: $b\na { n := { fresh: $missing } }") shouldBe
      List(UnresolvedSubstitution(at(3, 19), "missing"))
  }

  "Fields merged over a substitution" should "refer to each other and to what it found, as a map's fields do" in {
    val root = loaded("base { x: 1 }\na: $base\na { y: $a.x, z: ${a.y} }")
    root("a").toString shouldBe "x: 1\ny: 1\nz: 1"
    errors("base { x: 1 }\na: $base\na { y: $a.y }") shouldBe List(SubstitutionCycle(at(3, 8), "a.y"))
    errors("a: $missing\na { y: 1 }") shouldBe List(UnresolvedSubstitution(at(1, 4), "missing"))
    loaded("s: 1\na { y: 1 }\na: $s\nb: $a")("b") match {
      case Node.IScalar(_, _, "1", _) => succeed
      case other => fail(other.toString)
    }
  }

  "Layers within layers, which no source merges into" should "merge as the layers they hold, in turn" in {
    val site = at(1, 1)
    def map(key: String, value: String): Tree.Map =
      Tree.Map(Vector(key), scala.collection.immutable.Map(key -> Tree.Scalar(value, None, site)), None, site)
    def sub(key: String): Tree.Substitution = Tree.Substitution(Id.Key(key, Id.Root), optional = false, None, site)
    val layers = Tree.Layers(Vector(sub("b"), Tree.Layers(Vector(sub("c"), map("z", "3")))))
    val root = Tree.Map(
      Vector("b", "c", "a"),
      scala.collection.immutable.Map("b" -> map("x", "1"), "c" -> map("y", "2"), "a" -> layers),
      None,
      site)
    val resolved =
      Resolve(root, PsiLoader.DefaultDepthLimit, PsiLoader.DefaultNodeLimit, PsiLoader.DefaultCharacterLimit)
    resolved.errors shouldBe Nil
    Render.value(resolved.value.entries("a")) shouldBe "x: 1\ny: 2\nz: 3"
  }

  it should "refer to itself only through a cycle" in {
    errors("a: $a.x\na.x: 1") shouldBe List(SubstitutionCycle(at(1, 4), "a.x"))
  }

  it should "report what its substitutions do not find, and count what they copy" in {
    errors("a:\n  $nope*\n  $none*\na.x: 1") shouldBe
      List(UnresolvedSubstitution(at(2, 3), "nope"), UnresolvedSubstitution(at(3, 3), "none"))
    Psi.builder.withSizeLimit(1, PsiLoader.DefaultCharacterLimit).build.fold(e => fail(e.toString), identity)
      .parse("s", "b { x: 1, y: 2 }\na: $b\na.z: 1").fold(_.errors, root => fail(root.toString)) shouldBe
      List(ResolutionTooLarge(at(2, 4), "b", 1))
    Psi.parse("s", "a:\n  $?n*\n  $?m*\na.x: 1").fold(e => fail(e.toString), _("a").toString) shouldBe "x: 1"
  }

  "A cycle" should "be an error at the reference that closes it" in {
    errors("a: ${a}") shouldBe List(SubstitutionCycle(at(1, 4), "a"))
    errors("a: ${b}\nb: ${a}") shouldBe List(SubstitutionCycle(at(2, 4), "a"))
    errors("a: \"x${a}\"") shouldBe List(SubstitutionCycle(at(1, 6), "a"))
    errors("a: ${a.x}") shouldBe List(SubstitutionCycle(at(1, 4), "a.x"))
    errors("b { x: ${b} }") shouldBe List(SubstitutionCycle(at(1, 8), "b"))
    errors("s: [${?s[1]}, 1]") shouldBe List(SubstitutionCycle(at(1, 5), "s[1]"))
  }

  it should "be one from the end when the sequence needs the reference to know where its end is" in {
    errors("a: [x, $a[-1]*]") shouldBe List(CycleFromEnd(at(1, 8), "a[-1]"))
    errors("a: [x, $a[-2]*, y]") shouldBe List(CycleFromEnd(at(1, 8), "a[-2]"))
    errors("a: [x, $b*, y]\nb: $a[-1]") shouldBe List(CycleFromEnd(at(2, 4), "a[-1]"))
    // an element that is no injection is one, and referring to itself it is a cycle like any other
    errors("a: [x, $a[-1]]") shouldBe List(SubstitutionCycle(at(1, 8), "a[-1]"))
    // the last element contains the reference: a cycle, but not one of counting
    errors("a: [x, { k: $a[-1] }]") shouldBe List(SubstitutionCycle(at(1, 13), "a[-1]"))
    // an index from 0 needs only the elements before it
    loaded("a: [x, $a[0]]").toString shouldBe "a:\n  - x\n  - x\n"
  }

  it should "be reported over a failure beside it" in {
    errors("b { y: ${x}, x: ${b} }") shouldBe
      List(UnresolvedSubstitution(at(1, 8), "x"), SubstitutionCycle(at(1, 17), "b"))
  }

  "A path" should "be looked up in the tree alone, which the system properties do not reach" in {
    val properties = Map("java.home" -> "/j", "a" -> "launched")
    loaded("a: x\nt: ${a}", properties)("t") shouldBe Node.IScalar(keyed("t"), None, "x", ref(2, 4, "a", at(1, 4)))
    errors("h: ${java.home}\ni: ${HOME}", properties) shouldBe
      List(UnresolvedSubstitution(at(1, 4), "java.home"), UnresolvedSubstitution(at(2, 4), "HOME"))
  }

  "A substitution with no braces" should "graft and concatenate as one in braces does" in {
    // a `-` is part of a key, so `$a.b-y` would be the path `a.b-y`: braces end a path where it would go on
    val root = loaded("a { b: 1 }\nc: $a\nd: x$a.b/${a.b}-y\ne: [$?missing, $a.b]")
    root("c").toString shouldBe "b: 1"
    root("d") match {
      case Node.IScalar(_, _, value, _) => value shouldBe "x1/1-y"
      case other => fail(other.toString)
    }
    root("e").toString shouldBe "- 1"
  }

  "A graft" should "not take a substitution left unresolved, which is a bug of the loader" in {
    val substitution = Tree.Substitution(keyed("a"), optional = false, None, at(1, 4))
    the[IllegalStateException] thrownBy Resolve.graft(substitution, at(2, 4), keyed("b"), None) should have message
      "An unresolved substitution at s:1:4 grafted"
  }
}
