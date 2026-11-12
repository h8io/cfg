package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{FileNotFoundException, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipOutputStream}

class IncludeTest extends AnyFlatSpec with Matchers {
  private val NoTags = Set.empty[String]

  private def loader: PsiLoader =
    Psi.builder.withSystemProperties(Map.empty).build.fold(e => fail(e.toString), identity)

  private def write(dir: Path, name: String, text: String): Path = {
    val path = dir.resolve(name)
    Files.createDirectories(path.getParent)
    Files.write(path, text.getBytes(StandardCharsets.UTF_8))
  }

  private def url(path: Path): String = path.toUri.toURL.toString

  private def loaded(path: Path): Node.IMap[Id.Root] =
    loader(path.toUri.toURL).fold(errors => fail(errors.toString), identity)

  private def errors(path: Path): List[PsiError] = loader(path.toUri.toURL).fold(_.errors, root => fail(root.toString))

  private def scalar(root: Node.IMap[Id.Root], key: String): Node.ISome[Id] =
    root(key) match {
      case some: Node.ISome[?] => some
      case other => fail(other.toString)
    }

  "#include(file: …)" should "read a file relative to the including one, named as written" in {
    val dir = Files.createTempDirectory("psi")
    val main = write(dir, "main.psi", "a: 0\nb: 0\n#include(file: \"conf.d/extra.psi\")\nb: 2")
    write(dir, "conf.d/extra.psi", "a: 1\nb: 1\n#include(\"nested\")\nn: #include(file: 'nested.psi')")
    write(dir, "conf.d/nested.psi", "c: 3")
    val root = loaded(main)
    root("a") shouldBe Node.IScalar(Id.Key("a", Id.Root), NoTags, "1", SourceLocation("conf.d/extra.psi", 1, 4))
    root("b") shouldBe Node.IScalar(Id.Key("b", Id.Root), NoTags, "2", SourceLocation(url(main), 4, 4))
    root("c") shouldBe Node.IScalar(Id.Key("c", Id.Root), NoTags, "3", SourceLocation("nested", 1, 4))
    root("n") match {
      case map: Node.IMap[?] =>
        map.location shouldBe SourceLocation("nested.psi", 1, 1)
        map.size shouldBe 1
      case other => fail(other.toString)
    }
  }

  it should "look for a name with no extension as .Ψ, .ψ and .psi, in that order" in {
    val dir = Files.createTempDirectory("psi")
    write(dir, "x.Ψ", "v: upper")
    write(dir, "x.ψ", "v: lower")
    write(dir, "x.psi", "v: ascii")
    write(dir, "y.ψ", "v: lower")
    write(dir, "y.psi", "v: ascii")
    write(dir, "z.psi", "v: ascii")
    def value(name: String) =
      scalar(loaded(write(dir, "main.psi", "#include(file: \"" + name + "\")")), "v").toString
    value("x") should include("upper")
    value("y") should include("lower")
    value("z") should include("ascii")
    write(dir, "with space/w.psi", "v: spaced")
    value("with space/w.psi") should include("spaced")
  }

  it should "read an absolute path, and resolve a relative one in a text source against the working directory" in {
    val file = write(Files.createTempDirectory("psi"), "abs.psi", "v: 1")
    val relative = Paths.get("").toAbsolutePath.relativize(file).toString
    def v(text: String) = loader.parse("s", text).map(scalar(_, "v").toString)
    v("#include(file: \"" + file.toString + "\")").map(_.contains("1")) shouldBe Right(true)
    v("#include(file: \"" + relative + "\")").map(_.contains("1")) shouldBe Right(true)
  }

  it should "resolve against an entry of a jar" in {
    val jar = Files.createTempDirectory("psi").resolve("conf.jar")
    val out = new ZipOutputStream(Files.newOutputStream(jar))
    try
      for ((name, text) <- List("conf/a.psi" -> "#include(file: \"b\")", "conf/b.psi" -> "v: jarred")) {
        out.putNextEntry(new ZipEntry(name))
        out.write(text.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
      }
    finally out.close()
    val entry = java.net.URI.create("jar:" + jar.toUri.toString + "!/conf/a.psi").toURL
    loader(entry).map(scalar(_, "v")) shouldBe
      Right(Node.IScalar(Id.Key("v", Id.Root), NoTags, "jarred", SourceLocation("b", 1, 4)))
  }

  "#include(resource: …)" should "read from the class path, with the context class loader or Psi's own" in {
    def host(text: String) = loader.parse("s", text).map(_("server").isInstanceOf[Node.IMap[?]])
    host("#include(resource: \"base\")") shouldBe Right(true)
    val thread = Thread.currentThread
    val context = thread.getContextClassLoader
    thread.setContextClassLoader(null)
    try host("#include(resource: \"base.psi\")") shouldBe Right(true)
    finally thread.setContextClassLoader(context)
    loader.parse("s", "#include(resource: \"missing\", optional: true)").map(_.isEmpty) shouldBe Right(true)
  }

  "#include(url: …)" should "read an absolute URL as written, and refuse one that is not" in {
    val file = write(Files.createTempDirectory("psi"), "u.psi", "v: 1")
    loader.parse("s", "#include(url: \"" + url(file) + "\")").map(_.size) shouldBe Right(1)
    loader.parse("s", "#include(url: \"u.psi\")\n#include(url: \"a b:c\")").left.map(_.errors) shouldBe
      Left(
        List(
          DirectiveFailed(SourceLocation("s", 1, 1), "include", IncludeInvalidTarget("u.psi")),
          DirectiveFailed(SourceLocation("s", 2, 1), "include", IncludeInvalidTarget("a b:c"))
        ))
  }

  "An include" should "tolerate only a target not found when optional" in {
    val dir = Files.createTempDirectory("psi")
    val notZip = write(dir, "not.zip", "not a zip")
    val bytes = dir.resolve("bytes.psi")
    Files.write(bytes, Array[Byte]('a', ':', ' ', -1))
    val main = write(
      dir,
      "main.psi",
      "#include(file: \"missing\", optional: true)\n#include(file: \"missing\")\n" +
        "#include(url: \"jar:" + url(notZip) + "!/x.psi\", optional: true)\n" +
        "#include(file: \"bytes.psi\", optional: true)\n#include(file: \"x\", optional: yes)\n" +
        "#include(file: \"x\", url: \"y\")\n#include\n#include(file: \"a:b\")"
    )
    val location = SourceLocation(url(main), _: Int, 1)
    errors(main) match {
      case List(
            DirectiveFailed(l2, "include", IncludeNotFound("missing")),
            DirectiveFailed(l3, "include", IncludeUnreadable(target, cause)),
            InvalidEncoding(encoding),
            DirectiveFailed(l5, "include", IncludeOptional("yes")),
            DirectiveFailed(l6, "include", IncludeTargets(List("file", "url"))),
            DirectiveFailed(l7, "include", IncludeTargets(Nil)),
            DirectiveFailed(l8, "include", IncludeInvalidTarget("a:b"))
          ) =>
        List(l2, l3, l5, l6, l7, l8) shouldBe List(2, 3, 5, 6, 7, 8).map(location)
        target shouldBe "jar:" + url(notZip) + "!/x.psi"
        cause shouldBe an[IOException]
        cause should not be a[FileNotFoundException]
        encoding shouldBe SourceLocation("bytes.psi", 1, 4)
      case other => fail(other.toString)
    }
  }

  it should "be caught in a cycle" in {
    val dir = Files.createTempDirectory("psi")
    val a = write(dir, "a.psi", "#include(file: \"b\")")
    write(dir, "b.psi", "#include(file: \"a.psi\")")
    errors(a) shouldBe List(DirectiveCycle(SourceLocation("b", 1, 1), ::(url(a), List("b", "a.psi"))))
  }

  "Errors in an included source" should "come right after those of the source including it" in {
    val dir = Files.createTempDirectory("psi")
    val m = write(dir, "m.psi", "x: ${nope1}\n#include(file: \"i\")")
    write(dir, "i.psi", "y: ${nope2}")
    val n = write(dir, "n.psi", "z: ${nope3}")
    loader(m.toUri.toURL, n.toUri.toURL).left.map(_.errors) shouldBe
      Left(
        List(
          UnresolvedSubstitution(SourceLocation(url(m), 1, 4), "nope1"),
          UnresolvedSubstitution(SourceLocation("i", 1, 4), "nope2"),
          UnresolvedSubstitution(SourceLocation(url(n), 1, 4), "nope3")
        ))
  }

  "An include's errors" should "say what went wrong" in {
    IncludeTargets(List("file", "url")).message shouldBe
      "exactly one of file, resource and url is required, given [file, url]"
    IncludeOptional("yes").message shouldBe "optional is true or false, not yes"
    IncludeInvalidTarget("a b").message shouldBe "a b is no path or URL"
    IncludeNotFound("x").message shouldBe "x is not found"
    IncludeUnreadable("x", new IOException("boom")).message shouldBe "x cannot be read: java.io.IOException: boom"
  }
}
