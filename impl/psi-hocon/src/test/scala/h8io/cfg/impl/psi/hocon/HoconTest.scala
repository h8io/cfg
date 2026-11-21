package h8io.cfg.impl.psi.hocon

import com.sun.net.httpserver.HttpServer
import h8io.cfg.impl.psi.*
import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class HoconTest extends AnyFlatSpec with Matchers {
  private val loader: PsiLoader =
    Psi.builder.withDirective("hocon", Hocon).withSystemProperties(Map.empty).build.fold(e => fail(e.toString), x => x)

  private def directory(files: (String, String)*): Path = {
    val dir = Files.createTempDirectory("psi-hocon")
    files.foreach { case (name, text) => Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)) }
    dir
  }

  private def load(text: String, files: (String, String)*): Either[PsiErrors, Node.IMap[Id.Root]] =
    loader(directory((("app.psi" -> text) +: files)*).resolve("app.psi").toUri.toURL)

  private def loaded(text: String, files: (String, String)*): Node.IMap[Id.Root] =
    load(text, files*).fold(e => fail(e.message), x => x)

  private def errors(text: String, files: (String, String)*): List[PsiError] =
    load(text, files*).fold(_.errors, root => fail(root.toString))

  "#hocon" should "give a HOCON file's tree where it stands, its substitutions resolved inside it" in {
    // Typesafe Config keeps no order of keys
    def values(node: h8io.cfg.INode[Id]): Map[String, String] =
      node match {
        case map: Node.IMap[?] =>
          map.iterator.map {
            case Node.IScalar(id, _, value, _) => id.key -> value
            case other => fail(other.toString)
          }.toMap
        case other => fail(other.toString)
      }
    val root = loaded(
      "db: #hocon(file: \"db.conf\")\n#hocon(file: top)\nport: 1",
      "db.conf" -> "host: localhost\nurl: \"jdbc:\"${host}\ninclude \"more.conf\"",
      "more.conf" -> "pool: 10",
      "top.conf" -> "x: y\nport: 0"
    )
    values(root("db")) shouldBe Map("host" -> "localhost", "pool" -> "10", "url" -> "jdbc:localhost")
    root.iterator.map(_.id.key).toList.sorted shouldBe List("db", "port", "x")
    root("port") match {
      case Node.IScalar(_, _, "1", _) => succeed
      case other => fail(other.toString)
    }
  }

  it should "look for a file with no extension as .conf, .json and .properties, the first found read" in {
    loaded(
      "a: #hocon(file: x)\nb: #hocon(file: y)\nc: #hocon(file: z)",
      "x.conf" -> "v: conf",
      "x.json" -> "{\"v\": \"json\"}",
      "y.json" -> "{\"v\": \"json\"}",
      "y.properties" -> "v=properties",
      "z.properties" -> "v.w=properties"
    ).toString shouldBe
      "a:\n  v: conf\nb:\n  v: json\nc:\n  v:\n    w: properties\n"
  }

  it should "resolve a file with no system property, environment variable or reference.conf reaching it" in {
    val home = sys.props.contains("user.home") && sys.env.contains("PATH")
    home shouldBe true
    errors("a: #hocon(file: \"p.conf\")", "p.conf" -> "h: ${user.home}") match {
      case DirectiveFailed(_, "hocon", HoconInvalid(source, _)) :: Nil => source should endWith("/p.conf")
      case other => fail(other.toString)
    }
    errors("a: #hocon(file: \"e.conf\")", "e.conf" -> "h: ${PATH}") match {
      case DirectiveFailed(_, "hocon", HoconInvalid(source, _)) :: Nil => source should endWith("/e.conf")
      case other => fail(other.toString)
    }
    loaded("a: #hocon(file: \"o.conf\")", "o.conf" -> "h: ${?PATH}\nk: v").toString shouldBe "a:\n  k: v\n"
  }

  it should "give nothing for an optional file that is not there, and an error for one required or broken" in {
    loaded("a: #hocon(file: nope, optional: true)\nb: 1").toString shouldBe "b: 1\n"
    errors("a: #hocon(file: nope)") match {
      case DirectiveFailed(_, "hocon", IncludeNotFound("nope")) :: Nil => succeed
      case other => fail(other.toString)
    }
    errors("a: #hocon(file: \"bad.conf\")", "bad.conf" -> "a: [") match {
      case DirectiveFailed(_, "hocon", HoconInvalid(source, _)) :: Nil => source should endWith("/bad.conf")
      case other => fail(other.toString)
    }
    // a file it includes that is not there is no target that is not there
    errors(
      "a: #hocon(file: \"i.conf\", optional: true)",
      "i.conf" -> "include required(file(\"gone.conf\"))") match {
      case DirectiveFailed(_, "hocon", HoconInvalid(source, _)) :: Nil => source should endWith("/i.conf")
      case other => fail(other.toString)
    }
  }

  it should "find a URL from elsewhere not there only once it is read" in {
    val server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress, 0), 0)
    server.createContext(
      "/",
      { exchange =>
        val (status, body) = exchange.getRequestURI.getPath match {
          case "/ok.conf" => (200, "v: 1")
          case "/app.psi" => (200, "a: #hocon(file: data)\nb: #hocon(file: none, optional: true)")
          case "/data.json" => (200, "{\"v\": 2}")
          case "/fail.conf" => (500, "")
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
      def parse(text: String) = loader.parse("s", text)
      parse("a: #hocon(url: \"" + base + "/ok.conf\")").map(_.toString) shouldBe Right("a:\n  v: 1\n")
      // every extension asked for in turn, a URL's being known only once read
      loader(java.net.URI.create(base + "/app.psi").toURL).map(_.toString) shouldBe Right("a:\n  v: 2\n")
      parse("a: #hocon(url: \"" + base + "/no.conf\", optional: true)\nb: 1").map(_.toString) shouldBe
        Right("b: 1\n")
      parse("a: #hocon(url: \"" + base + "/no.conf\")").left.map(_.errors) match {
        case Left(DirectiveFailed(_, "hocon", IncludeNotFound(_)) :: Nil) => succeed
        case other => fail(other.toString)
      }
      parse("a: #hocon(url: \"" + base + "/fail.conf\", optional: true)").left.map(_.errors) match {
        case Left(DirectiveFailed(_, "hocon", HoconInvalid(_, _)) :: Nil) => succeed
        case other => fail(other.toString)
      }
    } finally server.stop(0)
  }

  "HoconInvalid" should "say what Typesafe Config said" in {
    HoconInvalid("a.conf", "broken").message shouldBe "a.conf cannot be read as HOCON: broken"
  }
}
