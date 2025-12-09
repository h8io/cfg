package h8io.cfg.raw.hocon

import h8io.cfg.raw.{Id, Node, Tag}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL

class ReaderTest extends AnyFlatSpec with Matchers {
  "package apply method" should "read application.conf with a empty URL list" in {
    val cfg = apply()
    cfg("application-config") should matchPattern {
      case Node.Scalar(Id.Key("application-config", Id.Root), "true", Tag.None(_), _) =>
    }
    cfg("base-config") should matchPattern { case Node.None(Id.Key("base-config", Id.Root), _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.Scalar(Id.Key("answer", Id.Root), "42", Tag.None(_), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), "true", Tag.None(_), _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), "reference", Tag.None(_), _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), "reference", Tag.None(_), _) => }
    cfg("property") should matchPattern { case Node.None(Id.Key("property", Id.Root), _) => }
    cfg("nullable") should matchPattern { case Node.None(Id.Key("nullable", Id.Root), _) => }
  }

  it should "read from a single URL" in {
    val cfg = apply(resource("base.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should
      matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), "true", Tag.None(_), _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), "true", Tag.None(_), _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), "reference", Tag.None(_), _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), "reference", Tag.None(_), _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), "12", Tag.None(_), _) => }
    cfg("nullable") should matchPattern { case Node.Null(Id.Key("nullable", Id.Root), _) => }
  }

  it should "read from two URLs" in {
    val cfg = apply(resource("base.conf"), resource("override.conf"))
    cfg("application-config") should matchPattern { case Node.None(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should
      matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), "true", Tag.None(_), _) => }
    cfg("override-config") should
      matchPattern { case Node.Scalar(Id.Key("override-config", Id.Root), "true", Tag.None(_), _) => }
    cfg("answer") should matchPattern { case Node.None(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.Scalar(Id.Key("reference", Id.Root), "true", Tag.None(_), _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), "overridden", Tag.None(_), _) => }
    cfg("src") should matchPattern { case Node.Scalar(Id.Key("src", Id.Root), "overridden", Tag.None(_), _) => }
    cfg("property") should
      matchPattern { case Node.Scalar(Id.Key("property", Id.Root), "42", Tag.Some("int", _), _) => }
    cfg("nullable") should matchPattern { case Node.Scalar(Id.Key("nullable", Id.Root), "non-null", Tag.None(_), _) => }
  }

  private def resource(path: String): URL = getClass.getClassLoader.getResource(path)
}
