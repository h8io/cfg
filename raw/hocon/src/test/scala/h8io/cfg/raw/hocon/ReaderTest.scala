package h8io.cfg.raw.hocon

import h8io.cfg.raw.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL

class ReaderTest extends AnyFlatSpec with Matchers {
  "package apply method" should "read application.conf with a empty URL list" in {
    val cfg = apply()
    cfg("application-config") should matchPattern {
      case Node.Scalar(Id.Key("application-config", Id.Root), "true", None, _) =>
    }
    cfg("base-config") should matchPattern { case Node.None(Id.Key("base-config", Id.Root), _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.Scalar(Id.Key("answer", Id.Root), "42", None, _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), "true", None, _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), "reference", None, _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), "reference", None, _) => }
    cfg("property") should matchPattern { case Node.None(Id.Key("property", Id.Root), _) => }
    cfg("nullable") should matchPattern { case Node.None(Id.Key("nullable", Id.Root), _) => }
  }

  it should "read from a single URL" in {
    val cfg = apply(resource("base.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), "true", None, _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), "true", None, _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), "reference", None, _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), "reference", None, _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), "12", None, _) => }
    cfg("nullable") should matchPattern { case Node.Null(Id.Key("nullable", Id.Root), _) => }
  }

  it should "read from two URLs" in {
    val cfg = apply(resource("base.conf"), resource("override.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), "true", None, _) => }
    cfg("override-config") should
      matchPattern { case Node.Scalar(Id.Key("override-config", Id.Root), "true", None, _) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), "true", None, _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), "overridden", None, _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), "overridden", None, _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), "42", Some("int"), _) => }
    cfg("nullable") should matchPattern { case Node.Scalar(Id.Key("nullable", Id.Root), "non-null", None, _) => }
  }

  private def resource(path: String): URL = getClass.getClassLoader.getResource(path)
}
