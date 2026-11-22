package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.net.{URL, URLClassLoader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util
import java.util.zip.{ZipEntry, ZipOutputStream}

class StandardTest extends AnyFlatSpec with Matchers {

  private val loader: PsiLoader =
    Psi.builder.withSystemProperties(Map.empty).build.fold(e => fail(e.toString), identity)

  /** A directory holding `files`, by name. */
  private def dir(files: (String, String)*): Path = {
    val dir = Files.createTempDirectory("psi")
    files.foreach { case (name, text) => Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)) }
    dir
  }

  /** A jar holding `files`, by name. */
  private def jar(files: (String, String)*): Path = {
    val jar = Files.createTempFile("psi", ".jar")
    val out = new ZipOutputStream(Files.newOutputStream(jar))
    try
      files.foreach { case (name, text) =>
        out.putNextEntry(new ZipEntry(name))
        out.write(text.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
      }
    finally out.close()
    jar
  }

  /** `f` with the thread's context class loader reading `classPath` alone, in order. */
  private def on[A](classPath: Path*)(f: => A): A =
    within(new URLClassLoader(classPath.map(_.toUri.toURL).toArray, null))(f)

  private def within[A](classLoader: ClassLoader)(f: => A): A = {
    val thread = Thread.currentThread
    val context = thread.getContextClassLoader
    thread.setContextClassLoader(classLoader)
    try f
    finally thread.setContextClassLoader(context)
  }

  private def text(result: Either[PsiErrors, Node.IMap[Id.Root]]): String =
    result.fold(errors => fail(errors.message), _.toString)

  "standard" should "load nothing from a class path with no reference and no application" in {
    on()(loader.standard().map(_.isEmpty)) shouldBe Right(true)
  }

  it should "merge every reference, .Ψ over .ψ over .psi, each in class path order, and the application over them" in {
    val first = dir("reference.psi" -> "a { x: 1, y: 1, z: 1 }\nurl: \"http://${a.host}:${a.y}\"")
    val second = jar("reference.psi" -> "a { y: 2, host: lib }", "reference.Ψ" -> "a.z: Ψ")
    val third = dir("reference.ψ" -> "a { y: ψ, z: ψ }", "application.ψ" -> "a.host: app", "application.psi" -> "b: 1")
    val fourth = dir("application.Ψ" -> "c: 1")
    text(on(first, second, third)(loader.standard())) shouldBe
      text(Psi.parse("x", "a { x: 1, y: ψ, z: Ψ, host: app }\nurl: \"http://app:ψ\""))
    text(on(fourth, third)(loader.standard())) shouldBe text(Psi.parse("x", "a { y: ψ, z: ψ }\nc: 1"))
  }

  it should "name a source by its URL, an entry of a jar too" in {
    val lib = jar("reference.psi" -> "a = 1")
    val url = "jar:" + lib.toUri.toURL.toString + "!/reference.psi"
    on(lib)(loader.standard()).left.map(_.errors.map(_.location)) shouldBe
      Left(List(SourceLocation(url, 1, 3)))
  }

  it should "report a class loader that cannot look for references, one error an extension" in {
    val broken = new ClassLoader(null) {
      override def getResources(name: String): util.Enumeration[URL] = throw new IOException(name)
    }
    within(broken)(loader.standard()).left.map(_.errors.map {
      case SourceUnreadable(source, cause) => source -> cause.getMessage
      case other => fail(other.toString)
    }) shouldBe Left(List("reference.psi", "reference.ψ", "reference.Ψ").map(name => name -> name))
  }

  "withReferences" should "load the sources given over every reference, in order" in {
    val lib = dir("reference.psi" -> "a { x: 1, y: 1 }", "application.psi" -> "ignored: 1")
    val app = dir("one.psi" -> "a.y: 2\nb: ${a.x}", "two.psi" -> "a.y: 3")
    text(
      on(lib)(loader.withReferences(app.resolve("one.psi").toUri.toURL, app.resolve("two.psi").toUri.toURL))) shouldBe
      text(Psi.parse("x", "a { x: 1, y: 3 }\nb: 1"))
    text(on(lib)(loader.withReferences())) shouldBe text(Psi.parse("x", "a { x: 1, y: 1 }"))
  }

  "Psi" should "load as a loader with nothing added does" in {
    val lib = dir("reference.psi" -> "a: 1", "application.psi" -> "b: 2")
    val app = dir("app.psi" -> "c: 3").resolve("app.psi").toUri.toURL
    text(on(lib)(Psi.standard())) shouldBe text(Psi.parse("x", "a: 1\nb: 2"))
    text(on(lib)(Psi.withReferences(app))) shouldBe text(Psi.parse("x", "a: 1\nc: 3"))
  }
}
