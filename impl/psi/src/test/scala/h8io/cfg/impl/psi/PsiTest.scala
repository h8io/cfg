package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileNotFoundException
import java.net.URL

class PsiTest extends AnyFlatSpec with Matchers {
  private def resource(name: String): URL = getClass.getClassLoader.getResource(name)

  private def at(url: URL, line: Int, column: Int): SourceLocation = SourceLocation(url.toString, line, column)

  private def loaded(result: Either[PsiErrors, Node.IMap[Id.Root]]): Node.IMap[Id.Root] =
    result.fold(errors => fail(errors.toString), identity)

  private def errors(result: Either[PsiErrors, Node.IMap[Id.Root]]): List[PsiError] =
    result.fold(_.errors, root => fail(root.toString))

  private val base = resource("base.psi")
  private val app = resource("app.psi")

  "Psi" should "give an empty root map, written nowhere, for no source" in {
    val root = loaded(Psi())
    root.id shouldBe Id.Root
    root.isEmpty shouldBe true
    root.tags shouldBe Set.empty
    root.location shouldBe NoSourceLocation
    root("a") shouldBe Node.None(Id.Key("a", Id.Root))
  }

  it should "load a source as written, every node at its id and its place" in {
    val root = loaded(Psi(base))
    root.location shouldBe at(base, 1, 1)
    root.map(_.id) shouldBe List("server", "pool", "list").map(Id.Key(_, Id.Root))
    val server = Id.Key("server", Id.Root)
    root("server") match {
      case map: Node.IMap[?] =>
        map.id shouldBe server
        map.location shouldBe at(base, 2, 8)
        map("host") shouldBe Node.IScalar(Id.Key("host", server), Set.empty, "localhost", at(base, 3, 9))
        map("user") shouldBe Node.None(Id.Key("user", server))
        map.toList shouldBe
          List(
            Node.IScalar(Id.Key("host", server), Set.empty, "localhost", at(base, 3, 9)),
            Node.IScalar(Id.Key("port", server), Set.empty, "8080", at(base, 4, 9)))
      case other => fail(other.toString)
    }
  }

  it should "view a sequence with its tags, its null and its maps" in {
    val list = Id.Key("list", Id.Root)
    loaded(Psi(base))("list") match {
      case seq: Node.ISeq[?] =>
        seq.tags shouldBe Set("t")
        seq.location shouldBe at(base, 7, 10)
        seq.size shouldBe 3
        seq(0) shouldBe Node.IScalar(Id.Index(0, list), Set.empty, "a", at(base, 7, 11))
        seq(1) shouldBe Node.INull(Id.Index(1, list), Set.empty, at(base, 7, 14))
        seq(2).id shouldBe Id.Index(2, list)
        seq(3) shouldBe Node.None(Id.Index(3, list))
        seq(-1) shouldBe Node.None(Id.Index(-1, list))
        seq.map(_.id) shouldBe List(0, 1, 2).map(Id.Index(_, list))
      case other => fail(other.toString)
    }
  }

  it should "remove a key from a map, and leave a map without it as it is" in {
    val root = loaded(Psi(base))
    val removed = root - "pool"
    removed.map(_.id) shouldBe List("server", "list").map(Id.Key(_, Id.Root))
    removed("pool") shouldBe Node.None(Id.Key("pool", Id.Root))
    (root - "absent") should be theSameInstanceAs root
  }

  it should "merge the sources in order, a later `:=` replacing, the merged maps at every site" in {
    val root = loaded(Psi(base, app))
    root.location shouldBe MergedLocation(at(app, 1, 1), ::(at(base, 1, 1), Nil))
    root.map(_.id) shouldBe List("server", "pool", "list", "name").map(Id.Key(_, Id.Root))
    val server = Id.Key("server", Id.Root)
    root("server") match {
      case map: Node.IMap[?] =>
        map.location shouldBe MergedLocation(at(app, 1, 1), ::(at(base, 2, 8), Nil))
        map("port") shouldBe Node.IScalar(Id.Key("port", server), Set.empty, "9090", at(app, 1, 14))
      case other => fail(other.toString)
    }
    root("pool") match {
      case map: Node.IMap[?] => map.map(_.id) shouldBe List(Id.Key("max", Id.Key("pool", Id.Root)))
      case other => fail(other.toString)
    }
  }

  it should "load a source held as text, under the name given" in {
    loaded(Psi.parse("inline", "a: 1"))("a") shouldBe
      Node.IScalar(Id.Key("a", Id.Root), Set.empty, "1", SourceLocation("inline", 1, 4))
  }

  "Errors" should "be reported for every source, an unreadable one included, in order" in {
    val broken = resource("broken.psi")
    val unclosed = resource("unclosed.psi")
    val missing = java.nio.file.Files.createTempDirectory("psi").resolve("missing.psi").toUri.toURL
    errors(Psi(broken, missing, unclosed)) match {
      case List(colon, space, SourceUnreadable(name, cause), bracket) =>
        colon shouldBe ExpectedColon(at(broken, 1, 3))
        space shouldBe SpaceInValue(at(broken, 3, 8))
        name shouldBe missing.toString
        cause shouldBe a[FileNotFoundException]
        bracket shouldBe UnclosedBracket(at(unclosed, 1, 3))
      case other => fail(other.toString)
    }
  }

  it should "come in two batches, syntax and directives first, merging and substitutions only without them" in {
    val directives = resource("directives.psi")
    val references = resource("references.psi")
    val conflict = resource("conflict.psi")
    errors(Psi(directives, references)) shouldBe
      List(
        NotSupportedYet(at(directives, 1, 1), "#include"),
        UnknownDirective(at(directives, 2, 5), "y"),
        UnknownDirective(at(directives, 3, 8), "z"))
    errors(Psi(resource("broken.psi"), references)) shouldBe
      List(ExpectedColon(at(resource("broken.psi"), 1, 3)), SpaceInValue(at(resource("broken.psi"), 3, 8)))
    errors(Psi(references)) shouldBe List(UnresolvedSubstitution(at(references, 4, 4), "psi-test-e"))
    errors(Psi(conflict)) shouldBe
      List(
        MergeWithSubstitution(at(conflict, 2, 4), at(conflict, 1, 3)),
        UnresolvedSubstitution(at(conflict, 3, 5), "psi-test-t"))
  }

  it should "come in the order of the sources in the second batch too, each source's by position" in {
    val first = resource("first.psi")
    val second = resource("second.psi")
    errors(Psi(first, second)) shouldBe
      List(
        UnresolvedSubstitution(at(first, 2, 4), "psi-test-x"),
        UnresolvedSubstitution(at(second, 1, 4), "psi-test-y"),
        MergeWithSubstitution(at(second, 2, 3), at(first, 3, 4))
      )
  }

  it should "resolve substitutions over the tree" in {
    val root = loaded(Psi.parse("s", "a: ${b}\nb: 1\nc: \"x${?psi-test-none}\""))
    root("a") shouldBe Node.IScalar(Id.Key("a", Id.Root), Set.empty, "1",
      ReferenceLocation(SourceLocation("s", 1, 4), "b", SourceLocation("s", 2, 4)))
    root("c") shouldBe Node.IScalar(Id.Key("c", Id.Root), Set.empty, "x", SourceLocation("s", 3, 4))
  }

  "A node" should "not be made of a substitution left unresolved, which is a bug of the loader" in {
    val substitution = Tree.Substitution(Id.Key("a", Id.Root), optional = false, Set.empty, SourceLocation("s", 1, 4))
    the[IllegalStateException] thrownBy wrap(Id.Root, substitution) should have message
      "An unresolved substitution at s:1:4"
  }
}
