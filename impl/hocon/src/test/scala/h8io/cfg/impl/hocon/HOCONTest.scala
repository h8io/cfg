package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigException
import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL
import java.nio.file.Files

class HOCONTest extends AnyFlatSpec with Matchers {
  "HOCON" should "read application.conf with a empty URL list" in {
    val cfg = HOCON()
    cfg("application-config") should matchPattern {
      case Node.Scalar(Id.Key("application-config", Id.Root), None, "true", _) =>
    }
    cfg("base-config") should matchPattern { case Node.None(Id.Key("base-config", Id.Root)) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root)) => }
    cfg("answer") should matchPattern { case Node.Scalar(Id.Key("answer", Id.Root), None, "42", _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), None, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "reference", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), None, "reference", _) => }
    cfg("property") should matchPattern { case Node.None(Id.Key("property", Id.Root)) => }
    cfg("nullable") should matchPattern { case Node.None(Id.Key("nullable", Id.Root)) => }
    cfg("yaml-config") should matchPattern { case Node.Scalar(Id.Key("yaml-config", Id.Root), None, "true", _) => }
    cfg("hocon-config") should matchPattern { case Node.Scalar(Id.Key("hocon-config", Id.Root), None, "true", _) => }
    cfg("json-config") should matchPattern { case Node.Scalar(Id.Key("json-config", Id.Root), None, "true", _) => }
    cfg("properties-config") should matchPattern {
      case Node.Scalar(Id.Key("properties-config", Id.Root), None, "true", _) =>
    }
  }

  it should "read from a single URL" in {
    val cfg = HOCON(resource("base.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root)) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), None, "true", _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root)) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root)) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), None, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "reference", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), None, "reference", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), None, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Null(Id.Key("nullable", Id.Root), None, _) => }
  }

  it should "read from two URLs" in {
    val cfg = HOCON(resource("base.conf"), resource("override.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root)) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), None, "true", _) => }
    cfg("override-config") should matchPattern {
      case Node.Scalar(Id.Key("override-config", Id.Root), None, "true", _) =>
    }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root)) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), None, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "overridden", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), None, "overridden", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), None, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Scalar(Id.Key("nullable", Id.Root), None, "non-null", _) => }
  }

  private def resource(path: String): URL = getClass.getClassLoader.getResource(path)

  it should "reject a URL that cannot be read, whatever its syntax" in
    Seq("conf", "json", "properties", "yaml").foreach { extension =>
      val url = Files.createTempDirectory("hocon").resolve(s"nonexistent.$extension").toUri.toURL
      a[ConfigException.IO] should be thrownBy HOCON(url)
    }

  it should "keep an include inside a URL optional" in {
    HOCON(resource("including.conf"))("including") should matchPattern {
      case Node.Scalar(Id.Key("including", Id.Root), None, "true", _) =>
    }
  }
}

class IsolatedTest extends AnyFlatSpec with Matchers {
  private def file(text: String): URL = {
    val path = Files.createTempFile("isolated", ".conf")
    Files.write(path, text.getBytes("UTF-8"))
    path.toUri.toURL
  }

  "HOCON.isolated" should "resolve a file's substitutions inside it" in {
    HOCON.isolated(file("a: x\nb: ${a}")).apply("b") match {
      case Node.IScalar(_, _, "x", _) => succeed
      case other => fail(other.toString)
    }
  }

  it should "reach no system property and no environment variable" in {
    sys.props.contains("user.home") shouldBe true
    a[ConfigException.UnresolvedSubstitution] should be thrownBy HOCON.isolated(file("h: ${user.home}"))
    a[ConfigException.UnresolvedSubstitution] should be thrownBy HOCON.isolated(file("h: ${PATH}"))
    HOCON.isolated(file("h: ${?PATH}")).size shouldBe 0
  }
}
