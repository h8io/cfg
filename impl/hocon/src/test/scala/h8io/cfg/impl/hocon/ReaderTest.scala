package h8io.cfg.impl.hocon

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL

class ReaderTest extends AnyFlatSpec with Matchers {
  "package apply method" should "read application.conf with a empty URL list" in {
    val cfg = apply()
    cfg("application-config") should matchPattern {
      case Node.Scalar(Id.Key("application-config", Id.Root), None, "true", _) =>
    }
    cfg("base-config") should matchPattern { case Node.None(Id.Key("base-config", Id.Root), _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.Scalar(Id.Key("answer", Id.Root), None, "42", _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), None, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "reference", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), None, "reference", _) => }
    cfg("property") should matchPattern { case Node.None(Id.Key("property", Id.Root), _) => }
    cfg("nullable") should matchPattern { case Node.None(Id.Key("nullable", Id.Root), _) => }
    cfg("yaml-config") should matchPattern { case Node.Scalar(Id.Key("yaml-config", Id.Root), None, "true", _) => }
    cfg("hocon-config") should matchPattern { case Node.Scalar(Id.Key("hocon-config", Id.Root), None, "true", _) => }
    cfg("json-config") should matchPattern { case Node.Scalar(Id.Key("json-config", Id.Root), None, "true", _) => }
    cfg("properties-config") should matchPattern {
      case Node.Scalar(Id.Key("properties-config", Id.Root), None, "true", _) =>
    }
  }

  it should "read from a single URL" in {
    val cfg = apply(resource("base.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), None, "true", _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), None, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "reference", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), None, "reference", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), None, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Null(Id.Key("nullable", Id.Root), None, _) => }
  }

  it should "read from two URLs" in {
    val cfg = apply(resource("base.conf"), resource("override.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), None, "true", _) => }
    cfg("override-config") should matchPattern {
      case Node.Scalar(Id.Key("override-config", Id.Root), None, "true", _) =>
    }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), None, "true", _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "overridden", _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), None, "overridden", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), None, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Scalar(Id.Key("nullable", Id.Root), None, "non-null", _) => }
  }

  private def resource(path: String): URL = getClass.getClassLoader.getResource(path)
}
