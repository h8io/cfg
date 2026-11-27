package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckTest extends AnyFlatSpec with Matchers {
  "check" should "find no error in a source with none" in {
    Psi.check("s", "a: 1\nb { c: [x, y] }\n") shouldBe Nil
  }

  it should "find the errors of the syntax, in the order of their lines and columns" in {
    val errors = Psi.check("s", "a: \"x\\q\" y\nb:c\n")
    errors.map(_.location) shouldBe errors.map(_.location).sortBy(l => (l.line, l.column))
    errors.map(_.location.line).distinct shouldBe List(1, 2)
    errors should not be empty
  }

  it should "give the errors parse gives for the syntax" in {
    val text = "a = 1\nb: [x,,y]\n"
    Psi.check("s", text) shouldBe Psi.parse("s", text).fold(_.errors, root => fail(root.toString))
  }

  it should "run no directive and resolve no substitution" in {
    Psi.check("s", "#include(file: \"missing\")*\na: ${nowhere}\nb: #env(name: NO_SUCH_VARIABLE_HERE)\n") shouldBe Nil
  }

  it should "report a marker and a formatter the loader does not know" in {
    val errors = Psi.check("s", "a: x\nb: \"\"\"bang\n  ${a; upper}\n  \"\"\"\n")
    errors.collect {
      case UnknownMarker(location, name) => (location.line, name)
      case UnknownFormatter(location, name) => (location.line, name)
    } shouldBe List(2 -> "bang", 3 -> "upper")
  }

  it should "know the markers and formatters of its loader" in {
    val marker = new Marker {
      def parameters: Seq[Parameter] = Nil
      def apply(arguments: Node.IMap[Id.Root], value: String): Either[MarkerError, String] = Right(value)
    }
    val formatter = new Formatter {
      def parameters: Seq[Parameter] = Nil
      def apply(arguments: Node.IMap[Id.Root]): Either[FormatterError, Node.Some => Either[FormatterError, String]] =
        Right(_ => Right(""))
    }
    val loader =
      Psi.builder.withMarker("bang", marker).withFormatter("upper", formatter).build.fold(e => fail(e.toString), x => x)
    loader.check("s", "a: x\nb: \"\"\"bang\n  ${a; upper}\n  \"\"\"\n") shouldBe Nil
  }

  it should "hold the tree to the loader's depth limit" in {
    val loader = Psi.builder.withDepthLimit(2).build.fold(e => fail(e.toString), x => x)
    loader.check("s", "a.b.c.d: 1\n") should not be empty
    Psi.check("s", "a.b.c.d: 1\n") shouldBe Nil
  }

  it should "name the source in its locations" in {
    Psi.check("app.psi", "a:b\n").map(_.location.source) shouldBe List("app.psi")
  }
}
