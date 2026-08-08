package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import h8io.cfg.{Id, Node}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.snakeyaml.engine.v2.exceptions.YamlEngineException

class YAMLTest extends AnyFlatSpec with Matchers with Inside {
  "YAML" should "return an empty root map for an empty URL list" in {
    val cfg = YAML()
    cfg shouldBe empty
    cfg("anything") should matchPattern { case Node.None(Id.Key("anything", Id.Root), _) => }
    cfg.location.description shouldBe LocationImpl.Unknown
  }

  it should "return an empty root map for an empty document" in {
    YAML(resource("empty.yaml")) shouldBe empty
  }

  it should "read from a single URL" in {
    val cfg = YAML(resource("base.yaml"))
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), None, "true", _) => }
    cfg("override-config") should matchPattern { case Node.None(Id.Key("override-config", Id.Root), _) => }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "base", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), None, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Null(Id.Key("nullable", Id.Root), None, _) => }
    inside(cfg("server")) { case server: Node.IMap[Id.Key] =>
      server("port") should matchPattern { case Node.Scalar(_, None, "8080", _) => }
      server("secure") should matchPattern { case Node.None(_, _) => }
    }
  }

  it should "name the URL in the location of every node it reads" in {
    val url = resource("base.yaml")
    inside(YAML(url)("property")) { case Node.Scalar(_, None, "12", location) =>
      location.description shouldBe s"$url: 3:11"
    }
  }

  it should "overlay two URLs, the last one winning" in {
    val cfg = YAML(resource("base.yaml"), resource("override.yaml"))
    cfg("base-config") should matchPattern { case Node.Scalar(Id.Key("base-config", Id.Root), None, "true", _) => }
    cfg("override-config") should matchPattern {
      case Node.Scalar(Id.Key("override-config", Id.Root), None, "true", _) =>
    }
    cfg("source") should matchPattern { case Node.Scalar(Id.Key("source", Id.Root), None, "overridden", _) => }
    cfg("property") should matchPattern { case Node.Scalar(Id.Key("property", Id.Root), None, "12", _) => }
    cfg("nullable") should matchPattern { case Node.Scalar(Id.Key("nullable", Id.Root), None, "non-null", _) => }
    inside(cfg("server")) { case server: Node.IMap[Id.Key] =>
      server("host") should matchPattern { case Node.Scalar(_, None, "localhost", _) => }
      server("port") should matchPattern { case Node.Scalar(_, None, "9090", _) => }
      server("secure") should matchPattern { case Node.Scalar(_, None, "true", _) => }
    }
    inside(cfg("hosts")) { case hosts: Node.ISeq[Id.Key] => hosts.size shouldBe 1 }
  }

  it should "report the tags written in the source" in {
    val cfg = YAML(resource("tags.yaml"))
    cfg("plain") should matchPattern { case Node.Scalar(Id.Key("plain", Id.Root), None, "42", _) => }
    cfg("tagged") should matchPattern {
      case Node.Scalar(Id.Key("tagged", Id.Root), Some("tag:yaml.org,2002:int"), "42", _) =>
    }
    cfg("non-specific") should matchPattern { case Node.Scalar(Id.Key("non-specific", Id.Root), None, "42", _) => }
    cfg("storage").asInstanceOf[Node.ISome[Id.Key]].tag shouldBe Some("!postgres")
    cfg("ids").asInstanceOf[Node.ISome[Id.Key]].tag shouldBe Some("!identifiers")
  }

  it should "reject a document that is not rooted at a mapping" in {
    val url = resource("root-seq.yaml")
    the[YamlEngineException] thrownBy YAML(url) should have message
      s"A configuration root must be a mapping, SEQUENCE found at $url: 1:1"
  }
}
