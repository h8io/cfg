package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResolveTest extends AnyFlatSpec with Matchers {
  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def ref(line: Int, column: Int, path: String, origin: PsiLocation): ReferenceLocation =
    ReferenceLocation(at(line, column), path, origin)

  private def load(
      text: String,
      properties: Map[String, String],
      env: Map[String, String]): Either[PsiErrors, Node.IMap[Id.Root]] =
    Psi.load(Right(Source("s", text)) :: Nil, () => properties, () => env)

  private def loaded(
      text: String,
      properties: Map[String, String] = Map.empty,
      env: Map[String, String] = Map.empty): Node.IMap[Id.Root] =
    load(text, properties, env).fold(errors => fail(errors.toString), identity)

  private def errors(
      text: String,
      properties: Map[String, String] = Map.empty,
      env: Map[String, String] = Map.empty): List[PsiError] =
    load(text, properties, env).fold(_.errors, root => fail(root.toString))

  private def keyed(names: String*): Id.Key =
    names.tail.foldLeft(Id.Key(names.head, Id.Root))((parent, name) => Id.Key(name, parent))

  private val NoTags = Set.empty[String]

  "A substitution" should "graft a scalar at its own position, located where it was taken from" in {
    loaded("a: ${b}\nb: 1")("a") shouldBe Node.IScalar(keyed("a"), NoTags, "1", ref(1, 4, "b", at(2, 4)))
  }

  it should "graft a map whole, every node of it at its new position, its tags united with those written" in {
    val root = loaded("a: @y ${b}\nb: @x { c: 1, d: [2] }")
    root("a") match {
      case map: Node.IMap[?] =>
        map.id shouldBe keyed("a")
        map.tags shouldBe Set("x", "y")
        map.location shouldBe ref(1, 7, "b", at(2, 7))
        map("c") shouldBe Node.IScalar(keyed("a", "c"), NoTags, "1", ref(1, 7, "b.c", at(2, 12)))
        map("d") match {
          case seq: Node.ISeq[?] =>
            seq.location shouldBe ref(1, 7, "b.d", at(2, 18))
            seq(0) shouldBe Node.IScalar(Id.Index(0, keyed("a", "d")), NoTags, "2", ref(1, 7, "b.d[0]", at(2, 19)))
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
    root("b") match {
      case map: Node.IMap[?] => map.tags shouldBe Set("x")
      case other => fail(other.toString)
    }
  }

  it should "chain the references of a chain of substitutions" in {
    loaded("a: ${b}\nb: ${c}\nc: 1")("a") shouldBe
      Node.IScalar(keyed("a"), NoTags, "1", ref(1, 4, "b", ref(2, 4, "c", at(3, 4))))
  }

  it should "graft null as null" in {
    loaded("a: ${n}\nn: null")("a") shouldBe Node.INull(keyed("a"), NoTags, ref(1, 4, "n", at(2, 4)))
  }

  it should "look into a sequence and below a substitution" in {
    val root = loaded("s: [0, { k: v }]\nt: ${s[1].k}\nu: ${w.k}\nw: ${s[1]}")
    root("t") shouldBe Node.IScalar(keyed("t"), NoTags, "v", ref(2, 4, "s[1].k", at(1, 13)))
    root("u") shouldBe Node.IScalar(keyed("u"), NoTags, "v", ref(3, 4, "w.k", ref(4, 4, "s[1].k", at(1, 13))))
  }

  "An optional substitution" should "drop its field when nothing is there, and graft what is" in {
    val root = loaded("a: ${?x}\nb: ${?c}\nc: 1")
    root("a") shouldBe Node.None(keyed("a"))
    root.size shouldBe 2
    root("b") shouldBe Node.IScalar(keyed("b"), NoTags, "1", ref(2, 4, "c", at(3, 4)))
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
    root("t") shouldBe Node.IScalar(keyed("t"), NoTags, "2", ref(2, 4, "s[1]", at(1, 22)))
    root("u") shouldBe Node.None(keyed("u"))
  }

  it should "count in a sequence when it grafts something" in {
    val root = loaded("s: [${?x}, 1]\nt: ${s[1]}\nu: ${s[0]}", env = Map("x" -> "0"))
    root("t") shouldBe Node.IScalar(keyed("t"), NoTags, "1", ref(2, 4, "s[1]", at(1, 12)))
    root("u") shouldBe Node.IScalar(keyed("u"), NoTags, "0", ref(3, 4, "s[0]", ref(1, 5, "x", EnvLocation("x"))))
  }

  it should "read as empty in a concatenation" in {
    loaded("e: \"${?x}\"")("e") shouldBe Node.IScalar(keyed("e"), NoTags, "", at(1, 4))
  }

  "A concatenation" should "join scalars, with a reference per substitution and only its own tags" in {
    loaded("h: @t db\np: 5432\nurl: @u \"jdbc:${h}:${p}/x${?none}\"")("url") shouldBe
      Node.IScalar(
        keyed("url"),
        Set("u"),
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
    errors("a: ${x}\nb: ${a}\nc: \"${a}\"\nd: [${a}]\ne { f: ${a} }\ng: ${e}", Map("a" -> "1")) shouldBe
      List(UnresolvedSubstitution(at(1, 4), "x"))
  }

  "A cycle" should "be an error at the reference that closes it" in {
    errors("a: ${a}") shouldBe List(SubstitutionCycle(at(1, 4), "a"))
    errors("a: ${b}\nb: ${a}") shouldBe List(SubstitutionCycle(at(2, 4), "a"))
    errors("a: \"x${a}\"") shouldBe List(SubstitutionCycle(at(1, 6), "a"))
    errors("a: ${a.x}") shouldBe List(SubstitutionCycle(at(1, 4), "a.x"))
    errors("b { x: ${b} }") shouldBe List(SubstitutionCycle(at(1, 8), "b"))
    errors("s: [${?s[1]}, 1]") shouldBe List(SubstitutionCycle(at(1, 5), "s[1]"))
  }

  it should "be reported over a failure beside it" in {
    errors("b { y: ${x}, x: ${b} }") shouldBe
      List(UnresolvedSubstitution(at(1, 8), "x"), SubstitutionCycle(at(1, 17), "b"))
  }

  "System properties" should "be looked up after the tree, split at the dots" in {
    val properties = Map("java.home" -> "/j", "java.version" -> "9", "a" -> "tree")
    val root = loaded("a: x\nh: ${java.home}\nj: ${java}\nt: ${a}", properties)
    root("h") shouldBe
      Node.IScalar(keyed("h"), NoTags, "/j", ref(2, 4, "java.home", SystemPropertyLocation("java.home")))
    root("j") match {
      case map: Node.IMap[?] =>
        map.location shouldBe ref(3, 4, "java", SystemPropertyLocation("java"))
        map.toList shouldBe
          List(
            Node.IScalar(keyed("j", "home"), NoTags, "/j", ref(3, 4, "java.home", SystemPropertyLocation("java.home"))),
            Node.IScalar(
              keyed("j", "version"),
              NoTags,
              "9",
              ref(3, 4, "java.version", SystemPropertyLocation("java.version")))
          )
      case other => fail(other.toString)
    }
    root("t") shouldBe Node.IScalar(keyed("t"), NoTags, "x", ref(4, 4, "a", at(1, 4)))
  }

  they should "come before the environment" in {
    loaded("h: ${HOME}", Map("HOME" -> "p"), Map("HOME" -> "e"))("h") shouldBe
      Node.IScalar(keyed("h"), NoTags, "p", ref(1, 4, "HOME", SystemPropertyLocation("HOME")))
  }

  they should "not be read when the tree has everything" in {
    load("a: ${b}\nb: 1", Map.empty, Map.empty).isRight shouldBe true
    Psi.load(Right(Source("s", "a: ${b}\nb: 1")) :: Nil, () => fail("read"), () => fail("read")).isRight shouldBe true
  }

  they should "lose a value that is also a prefix to the map" in {
    Resolve.properties(Map("a" -> "1", "a.b" -> "2", "c..d" -> "3")) shouldBe
      Tree.Map(
        Vector("a", "c"),
        Map(
          "a" -> Tree.Map(
            Vector("b"),
            Map("b" -> Tree.Scalar("2", NoTags, SystemPropertyLocation("a.b"))),
            NoTags,
            SystemPropertyLocation("a")),
          "c" -> Tree.Map(
            Vector(""),
            Map(
              "" -> Tree.Map(
                Vector("d"),
                Map("d" -> Tree.Scalar("3", NoTags, SystemPropertyLocation("c..d"))),
                NoTags,
                SystemPropertyLocation("c."))),
            NoTags,
            SystemPropertyLocation("c")
          )
        ),
        NoTags,
        NoSourceLocation
      )
  }

  "The environment" should "be looked up last, not split" in {
    val env = Map("HOME" -> "/h", "a.b" -> "v")
    val root = loaded("h: ${HOME}\nq: ${`a.b`}", env = env)
    root("h") shouldBe Node.IScalar(keyed("h"), NoTags, "/h", ref(1, 4, "HOME", EnvLocation("HOME")))
    root("q") shouldBe Node.IScalar(keyed("q"), NoTags, "v", ref(2, 4, "`a.b`", EnvLocation("a.b")))
    errors("x: ${a.b}\ny: ${HOME[0]}", env = env) shouldBe
      List(UnresolvedSubstitution(at(1, 4), "a.b"), UnresolvedSubstitution(at(2, 4), "HOME[0]"))
  }

  "A graft" should "not take a substitution left unresolved, which is a bug of the loader" in {
    val substitution = Tree.Substitution(keyed("a"), optional = false, NoTags, at(1, 4))
    the[IllegalStateException] thrownBy Resolve.graft(substitution, at(2, 4), keyed("b"), NoTags) should have message
      "An unresolved substitution at s:1:4 grafted"
  }

  "A map" should "not be merged once grafted, which is a bug of the loader" in {
    the[IllegalStateException] thrownBy Merge.again(at(2, 1), ref(1, 4, "b", at(1, 1))) should have message
      "A map at s:1:4 (${b} from s:1:1) is merged"
  }
}
