package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigException
import h8io.cfg.impl.hocon.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL
import java.nio.file.Files

class HOCONTest extends AnyFlatSpec with Matchers {
  "HOCON" should "read application.conf with a empty URL list" in {
    val cfg = HOCON()
    cfg("application-config") should matchPattern {
      case Node.Scalar(Id.Key("application-config", Id.Root), NoTags, "true", _) =>
    }
    cfg("base-config") should matchPattern { case Node.None(Id.Key("base-config", Id.Root)) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root)) => }
    cfg("answer") should matchPattern { case Node.Scalar(Id.Key("answer", Id.Root), NoTags, "42", _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), NoTags, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), NoTags, "reference", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), NoTags, "reference", _) => }
    cfg("property") should matchPattern { case Node.None(Id.Key("property", Id.Root)) => }
    cfg("nullable") should matchPattern { case Node.None(Id.Key("nullable", Id.Root)) => }
    cfg("yaml-config") should matchPattern { case Node.Scalar(Id.Key("yaml-config", Id.Root), NoTags, "true", _) => }
    cfg("hocon-config") should matchPattern { case Node.Scalar(Id.Key("hocon-config", Id.Root), NoTags, "true", _) => }
    cfg("json-config") should matchPattern { case Node.Scalar(Id.Key("json-config", Id.Root), NoTags, "true", _) => }
    cfg("properties-config") should matchPattern {
      case Node.Scalar(Id.Key("properties-config", Id.Root), NoTags, "true", _) =>
    }
  }

  it should "read from a single URL" in {
    val cfg = HOCON(resource("base.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root)) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), NoTags, "true", _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root)) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root)) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), NoTags, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), NoTags, "reference", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), NoTags, "reference", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), NoTags, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Null(Id.Key("nullable", Id.Root), NoTags, _) => }
  }

  it should "read from two URLs" in {
    val cfg = HOCON(resource("base.conf"), resource("override.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root)) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), NoTags, "true", _) => }
    cfg("override-config") should matchPattern {
      case Node.Scalar(Id.Key("override-config", Id.Root), NoTags, "true", _) =>
    }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root)) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), NoTags, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), NoTags, "overridden", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), NoTags, "overridden", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), NoTags, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Scalar(Id.Key("nullable", Id.Root), NoTags, "non-null", _) => }
  }

  private def resource(path: String): URL = getClass.getClassLoader.getResource(path)

  it should "reject a URL that cannot be read, whatever its syntax" in
    Seq("conf", "json", "properties", "yaml").foreach { extension =>
      val url = Files.createTempDirectory("hocon").resolve(s"nonexistent.$extension").toUri.toURL
      a[ConfigException.IO] should be thrownBy HOCON(url)
    }

  it should "keep an include inside a URL optional" in {
    HOCON(resource("including.conf"))("including") should matchPattern {
      case Node.Scalar(Id.Key("including", Id.Root), NoTags, "true", _) =>
    }
  }
}
