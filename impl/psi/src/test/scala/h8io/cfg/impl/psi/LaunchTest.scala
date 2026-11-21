package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LaunchTest extends AnyFlatSpec with Matchers {

  private def at(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private val properties = Map(
    "app.port" -> "8080",
    "java.version" -> "25",
    "java.vm.name" -> "HotSpot",
    "java.vm.vendor" -> "Oracle",
    "a" -> "1",
    "a.b" -> "2",
    "c.d" -> "3",
    "c.d.e" -> "4")

  private def loader: PsiLoader =
    Psi.builder
      .withSystemProperties(properties)
      .withEnvironment(Map("HOME" -> "/home/x"))
      .build
      .fold(e => fail(e.toString), identity)

  private def loaded(text: String): Node.IMap[Id.Root] = loader.parse("s", text).fold(e => fail(e.toString), identity)

  private def errors(text: String): List[PsiError] = loader.parse("s", text).fold(_.errors, root => fail(root.toString))

  private def value(node: Node): String =
    node match {
      case Node.IScalar(_, _, value, _) => value
      case other => fail(other.toString)
    }

  "#prop" should "give a system property, located at the call and at the property" in {
    loaded("port: #prop(\"app.port\")")("port") shouldBe
      Node.IScalar(
        Id.Key("port", Id.Root),
        None,
        "8080",
        DirectiveLocation(at(1, 7), "prop", SystemPropertyLocation("app.port")))
  }

  it should "give a map of the properties under a name no property has, split at their dots" in {
    loaded("java: #prop(java)")("java").toString shouldBe
      "version: 25\nvm:\n  name: HotSpot\n  vendor: Oracle"
    // in statement position, its keys stand in the block
    loaded("x { #prop(java) }")("x").toString should include("version: 25")
  }

  it should "give the property itself when a name is a property and has properties under it too" in {
    value(loaded("a: #prop(a)")("a")) shouldBe "1"
  }

  it should "fail on properties under the name one of which is a value and has properties under it too" in {
    errors("c: #prop(c)") shouldBe List(DirectiveFailed(at(1, 4), "prop", PropertyConflict("c.d")))
  }

  "#env" should "give an environment variable, located at the call and at the variable" in {
    loaded("home: #env(HOME)")("home") shouldBe
      Node.IScalar(
        Id.Key("home", Id.Root),
        None,
        "/home/x",
        DirectiveLocation(at(1, 7), "env", EnvironmentLocation("HOME")))
  }

  "A value not set" should "be an error, nothing when optional, and the default when one is given" in {
    errors("a: #prop(nope)\nb: #env(NOPE)") shouldBe
      List(
        DirectiveFailed(at(1, 4), "prop", PropertyNotSet("nope")),
        DirectiveFailed(at(2, 4), "env", VariableNotSet("NOPE")))
    // nothing drops the field, so that an earlier definition of the key stands
    value(loaded("port: 80\nport: #prop(nope, optional: true)")("port")) shouldBe "80"
    loaded("port: #env(NOPE, optional: false, default: 80)")("port") shouldBe
      Node.IScalar(Id.Key("port", Id.Root), None, "80", DirectiveLocation(at(1, 7), "env", at(1, 44)))
    // a value set wins over the default
    value(loaded("port: #prop(\"app.port\", default: 80)")("port")) shouldBe "8080"
  }

  "The arguments" should "take optional as true or false, and not together with a default" in {
    errors("a: #prop(a, optional: yes)\nb: #env(E, optional: true, default: x)") shouldBe
      List(
        DirectiveFailed(at(1, 4), "prop", LaunchOptional("yes")),
        DirectiveFailed(at(2, 4), "env", OptionalWithDefault))
  }

  "The built-in names" should "be taken by no handler, and turn off as any directive does" in {
    val handler = new Directive {
      def parameters: Seq[Parameter] = Nil
      def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
        Right(Fragment.None)
    }
    Psi.builder.withDirective("env", handler).build.left.map(_.errors) shouldBe Left(::(DuplicateDirective("env"), Nil))
    Psi.builder.disableDirective("env").build.fold(e => fail(e.toString), identity).parse("s", "a: #env(HOME)") shouldBe
      Left(PsiErrors(::(DisabledDirective(at(1, 4), "env"), Nil)))
  }

  "A loader given no maps" should "read the JVM's properties and environment" in {
    val root =
      Psi.parse("s", "v: #prop(\"java.version\")\np: #env(PATH, default: none)").fold(e => fail(e.toString), identity)
    value(root("v")) shouldBe sys.props("java.version")
    value(root("p")) shouldBe sys.env.getOrElse("PATH", "none")
  }

  "Their errors" should "say what is not set, and what to do" in {
    val hint = Some("set it at launch, or give the call optional: true or a default")
    (PropertyNotSet("a.b").message, PropertyNotSet("a.b").hint) shouldBe
      (("the system property 'a.b' is not set, nor any under it", hint))
    (VariableNotSet("E").message, VariableNotSet("E").hint) shouldBe (("the environment variable 'E' is not set", hint))
    LaunchOptional("yes").message shouldBe "optional is true or false, not yes"
    (OptionalWithDefault.message, OptionalWithDefault.hint) shouldBe
      (("optional and default cannot both be given", Some("keep the default, which is used when the value is not set")))
    PropertyConflict("c.d").message shouldBe "the system property 'c.d' has a value and properties under it"
    PropertyConflict("c.d").hint shouldBe None
  }
}
