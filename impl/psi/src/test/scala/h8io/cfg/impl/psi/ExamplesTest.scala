package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The examples under `examples/` in the test resources, each a directory: its sources, loaded in the order of their
  * names, and what they load as, written back as Psi in `expected.psi`, or the report of their errors in
  * `expected.errors`, sources named relative to the directory.
  */
class ExamplesTest extends AnyFlatSpec with Matchers {
  private val Tree = "expected.psi"
  private val Errors = "expected.errors"

  private val examples: List[Path] = {
    val uri = getClass.getResource("/examples").toURI
    // the resources may be packed in a jar, as sbt 2 packs them, whose file system is opened once and kept open
    if (uri.getScheme == "jar") {
      val _ = FileSystems.newFileSystem(uri, java.util.Collections.emptyMap[String, Any]())
    }
    val root = Paths.get(uri)
    val listed = Files.list(root)
    try listed.iterator.asScala.filter(Files.isDirectory(_)).toList.sortBy(_.getFileName.toString)
    finally listed.close()
  }

  private def sources(example: Path): List[Path] = {
    val listed = Files.list(example)
    try
      listed.iterator.asScala
        .filter(path => path.getFileName.toString.endsWith(".psi") && path.getFileName.toString != Tree)
        .toList
        .sortBy(_.getFileName.toString)
    finally listed.close()
  }

  private def read(path: Path): Option[String] =
    if (Files.exists(path)) Some(new String(Files.readAllBytes(path), StandardCharsets.UTF_8)) else None

  "The examples" should "be there" in {
    examples should not be empty
  }

  examples.foreach { example =>
    val name = example.getFileName.toString

    ("Example " + name) should "load as expected" in {
      sources(example) should not be empty
      Psi(sources(example).map(_.toUri.toURL)*) match {
        case Right(root) =>
          val written = root.toString
          withClue(name + " loads as\n" + written) {
            read(example.resolve(Errors)) shouldBe None
            read(example.resolve(Tree)) shouldBe Some(written)
          }
          // what is written back loads back as itself
          Psi.parse(Tree, written).map(_.toString) shouldBe Right(written)
        case Left(errors) =>
          val report = errors.message.replace(example.toUri.toString.stripSuffix("/") + "/", "") + "\n"
          withClue(name + " reports\n" + report) {
            read(example.resolve(Tree)) shouldBe None
            read(example.resolve(Errors)) shouldBe Some(report)
          }
      }
    }
  }
}
