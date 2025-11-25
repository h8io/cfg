package h8io.cfg.raw.hocon

import h8io.cfg.raw.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URL

class ReaderTest extends AnyFlatSpec with Matchers {
  "package apply method" should "read application.conf with a empty URL list" in {
    val cfg = apply()
    cfg("application-config") should matchPattern {
      case Node.IScalar(Id.Key("application-config", Id.Root), "true", _) =>
    }
    cfg("base-config") should matchPattern { case Node.INone(Id.Key("base-config", Id.Root), _) => }
    cfg("override-config") should matchPattern { case Node.INone(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.IScalar(Id.Key("answer", Id.Root), "42", _) => }
    cfg("reference") should matchPattern { case Node.IScalar(Id.Key("reference", Id.Root), "true", _) => }
    cfg("source") should matchPattern { case Node.IScalar(Id.Key("source", Id.Root), "reference", _) => }
    cfg("src") should matchPattern { case Node.IScalar(Id.Key("src", Id.Root), "reference", _) => }
    cfg("property") should matchPattern { case Node.INone(Id.Key("property", Id.Root), _) => }
    cfg("nullable") should matchPattern { case Node.INone(Id.Key("nullable", Id.Root), _) => }
  }

  it should "read from a single URL" in {
    val cfg = apply(resource("base.conf"))
    cfg("application-config") should matchPattern { case Node.INone(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should matchPattern { case Node.IScalar(Id.Key("base-config", Id.Root), "true", _) => }
    cfg("override-config") should matchPattern { case Node.INone(Id.Key("override-config", Id.Root), _) => }
    cfg("answer") should matchPattern { case Node.INone(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.IScalar(Id.Key("reference", Id.Root), "true", _) => }
    cfg("source") should matchPattern { case Node.IScalar(Id.Key("source", Id.Root), "reference", _) => }
    cfg("src") should matchPattern { case Node.IScalar(Id.Key("src", Id.Root), "reference", _) => }
    cfg("property") should matchPattern { case Node.IScalar(Id.Key("property", Id.Root), "12", _) => }
    cfg("nullable") should matchPattern { case Node.INull(Id.Key("nullable", Id.Root), _) => }
  }

  it should "read from two URLs" in {
    val cfg = apply(resource("base.conf"), resource("override.conf"))
    cfg("application-config") should matchPattern { case Node.INone(Id.Key("application-config", Id.Root), _) => }
    cfg("base-config") should matchPattern { case Node.IScalar(Id.Key("base-config", Id.Root), "true", _) => }
    cfg("override-config") should matchPattern { case Node.IScalar(Id.Key("override-config", Id.Root), "true", _) => }
    cfg("answer") should matchPattern { case Node.INone(Id.Key("answer", Id.Root), _) => }
    cfg("reference") should matchPattern { case Node.IScalar(Id.Key("reference", Id.Root), "true", _) => }
    cfg("source") should matchPattern { case Node.IScalar(Id.Key("source", Id.Root), "overridden", _) => }
    cfg("src") should matchPattern { case Node.IScalar(Id.Key("src", Id.Root), "overridden", _) => }
    cfg("property") should matchPattern { case Node.IScalar(Id.Key("property", Id.Root), "42", _) => }
    cfg("nullable") should matchPattern { case Node.IScalar(Id.Key("nullable", Id.Root), "non-null", _) => }
  }

  private def resource(path: String): URL = getClass.getClassLoader.getResource(path)
}
