package h8io.cfg.impl.psi.yaml

import com.sun.net.httpserver.HttpServer
import h8io.cfg.impl.psi.*
import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

class YamlTest extends AnyFlatSpec with Matchers {
  private def loader(build: PsiBuilder => PsiBuilder = identity): PsiLoader =
    build(Psi.builder.withDirective("yaml", Yaml).withSystemProperties(Map.empty)).build
      .fold(e => fail(e.toString), x => x)

  private def directory(files: (String, String)*): Path = {
    val dir = Files.createTempDirectory("psi-yaml")
    files.foreach { case (name, text) => Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)) }
    dir
  }

  /** Loads `app.psi`, holding `text`, from a directory with `files` beside it. */
  private def load(text: String, files: (String, String)*): Either[PsiErrors, Node.IMap[Id.Root]] = {
    val dir = directory((("app.psi" -> text) +: files)*)
    loader()(dir.resolve("app.psi").toUri.toURL)
  }

  private def loaded(text: String, files: (String, String)*): Node.IMap[Id.Root] =
    load(text, files*).fold(e => fail(e.message), x => x)

  private def errors(text: String, files: (String, String)*): List[PsiError] =
    load(text, files*).fold(_.errors, root => fail(root.toString))

  "#yaml" should "give a YAML file's tree where it stands, relative to the source, its tags as written" in {
    val root = loaded(
      "db: #yaml(file: \"db.yaml\")\n#yaml(file: top)*\nport: 1",
      "db.yaml" -> "host: !h localhost\nports: [1, 2]\n",
      "top.yaml" -> "x: y\nport: 0\n")
    root.toString shouldBe "db:\n  host: @h localhost\n  ports:\n    - 1\n    - 2\nx: y\nport: 1\n"
    root("db") match {
      case db: Node.IMap[?] =>
        db("host") match {
          case Node.IScalar(_, _, _, DirectiveLocation(at, "yaml", origin)) =>
            at.line shouldBe 1
            origin.description should endWith("db.yaml: 1:7")
          case other => fail(other.toString)
        }
      case other => fail(other.toString)
    }
  }

  it should "take any node at the root, an empty file an empty map" in {
    val root = loaded(
      "s: #yaml(file: \"s.yaml\")\nv: #yaml(file: \"v.yaml\")\ne: #yaml(file: \"e.yaml\")",
      "s.yaml" -> "- a\n- b\n",
      "v.yaml" -> "word\n",
      "e.yaml" -> "")
    root.toString shouldBe "s:\n  - a\n  - b\nv: word\ne: {}\n"
  }

  it should "look for a file with no extension as .yaml, then .yml" in {
    loaded("a: #yaml(file: x)\nb: #yaml(file: y)", "x.yaml" -> "1", "x.yml" -> "2", "y.yml" -> "3").toString shouldBe
      "a: 1\nb: 3\n"
  }

  it should "give nothing for an optional file that is not there, and an error for one that is required" in {
    loaded("a: #yaml(file: nope, optional: true)\nb: 1").toString shouldBe "b: 1\n"
    errors("a: #yaml(file: nope)") match {
      case DirectiveFailed(_, "yaml", IncludeNotFound("nope")) :: Nil => succeed
      case other => fail(other.toString)
    }
    errors("a: #yaml(file: x, url: y)") match {
      case DirectiveFailed(_, "yaml", IncludeTargets(List("file", "url"))) :: Nil => succeed
      case other => fail(other.toString)
    }
  }

  it should "fail on a file that is no YAML, or cannot be read" in {
    errors("a: #yaml(file: \"bad.yaml\")", "bad.yaml" -> "a: [") match {
      case DirectiveFailed(_, "yaml", YamlInvalid(source, reason)) :: Nil =>
        source should endWith("/bad.yaml")
        reason should include("bad.yaml")
      case other => fail(other.toString)
    }
    val dir = directory("app.psi" -> "a: #yaml(file: \"locked.yaml\")", "locked.yaml" -> "a: 1")
    val locked = dir.resolve("locked.yaml")
    Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("---------"))
    try
      loader()(dir.resolve("app.psi").toUri.toURL).fold(_.errors, root => fail(root.toString)) match {
        case DirectiveFailed(_, "yaml", IncludeUnreadable("locked.yaml", _)) :: Nil => succeed
        case other => fail(other.toString)
      }
    finally Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("rw-------"))
  }

  it should "read through the load's read limit" in {
    val dir = directory("app.psi" -> "a: #yaml(file: \"big.yaml\")", "big.yaml" -> ("x: " + "y" * 100))
    val limited = loader(_.withReadLimit(50, java.time.Duration.ofSeconds(60)))
    limited(dir.resolve("app.psi").toUri.toURL).fold(_.errors, root => fail(root.toString)) match {
      case ReadLimitExceeded(source, 50) :: Nil => source should endWith("big.yaml")
      case other => fail(other.toString)
    }
  }

  it should "find a URL from elsewhere not there only once it is read" in {
    val server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress, 0), 0)
    server.createContext(
      "/",
      { exchange =>
        val (status, body) = exchange.getRequestURI.getPath match {
          case "/ok.yaml" => (200, "v: 1")
          case "/app.psi" => (200, "a: #yaml(file: data, optional: true)\nb: #yaml(file: none, optional: true)")
          case "/data.yml" => (200, "v: 2")
          case "/fail.yaml" => (500, "")
          case _ => (404, "")
        }
        val bytes = body.getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(status, if (bytes.isEmpty) -1L else bytes.length.toLong)
        if (bytes.nonEmpty) exchange.getResponseBody.write(bytes)
        exchange.close()
      }
    )
    server.start()
    try {
      val base = "http://" + server.getAddress.getHostString + ":" + server.getAddress.getPort.toString
      def parse(text: String) = loader().parse("s", text)
      parse("a: #yaml(url: \"" + base + "/ok.yaml\")").map(_.toString) shouldBe Right("a:\n  v: 1\n")
      // every extension asked for in turn, a URL's being known only once read
      loader()(java.net.URI.create(base + "/app.psi").toURL).map(_.toString) shouldBe Right("a:\n  v: 2\n")
      parse("a: #yaml(url: \"" + base + "/no.yaml\", optional: true)\nb: 1").map(_.toString) shouldBe Right("b: 1\n")
      parse("a: #yaml(url: \"" + base + "/no.yaml\")").left.map(_.errors) match {
        case Left(DirectiveFailed(_, "yaml", IncludeNotFound(_)) :: Nil) => succeed
        case other => fail(other.toString)
      }
      parse("a: #yaml(url: \"" + base + "/fail.yaml\", optional: true)").left.map(_.errors) match {
        case Left(DirectiveFailed(_, "yaml", IncludeUnreadable(_, _)) :: Nil) => succeed
        case other => fail(other.toString)
      }
    } finally server.stop(0)
  }

  it should "count every use of an alias against the node limit, before building it" in {
    // each level four uses of the one before, 48 aliases, within snakeyaml-engine's 50: 4¹² leaves from 400 bytes
    def ref(i: Int): String = if (i == 1) "a" else "b" + (i - 1).toString
    val bomb = "a: &a [x, x, x, x]\n" +
      (1 to 12).map(i =>
        "b" + i.toString + ": &b" + i.toString + " [" + List.fill(4)("*" + ref(i)).mkString(", ") + "]")
        .mkString("\n")
    val dir = directory("app.psi" -> "a: #yaml(file: bomb)\nb: #yaml(file: bomb)", "bomb.yaml" -> bomb)
    loader(_.withSizeLimit(1000, 1000))(dir.resolve("app.psi").toUri.toURL).fold(
      _.errors, root => fail(root.toString)) match {
      case FragmentTooLarge(at, "yaml", 1000) :: Nil => at.line shouldBe 1
      case other => fail(other.toString)
    }
  }

  it should "fail on a merge key that makes a mapping contain itself, before snakeyaml-engine follows it" in {
    errors("a: #yaml(file: \"m.yaml\")", "m.yaml" -> "a: &a {<<: *a}") match {
      case DirectiveFailed(_, "yaml", YamlInvalid(_, reason)) :: Nil => reason should include("contain itself")
      case other => fail(other.toString)
    }
  }

  "YamlInvalid" should "say what the parser said" in {
    YamlInvalid("a.yaml", "broken").message shouldBe "a.yaml cannot be read as YAML: broken"
  }
}
