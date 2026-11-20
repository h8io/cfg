package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.Token.{Reference, Text}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.CharBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import scala.collection.immutable

class RenderTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private val nowhere = SourceLocation("", 1, 1)

  /** The merged tree of `text`, with no error. */
  private def tree(text: String): Tree.Map = {
    val parsed = Parser.parse(Source("s", text))
    parsed.errors shouldBe Nil
    val merged = Merge(::((parsed.value, nowhere), Nil))
    merged.errors shouldBe Nil
    merged.value
  }

  /** `value` with every location the same, to compare trees by what they hold. */
  private def located(value: Tree.Value): Tree.Value =
    value match {
      case Tree.Scalar(text, tags, _) => Tree.Scalar(text, tags, nowhere)
      case Tree.Null(tags, _) => Tree.Null(tags, nowhere)
      case Tree.Sequence(elements, tags, _) => Tree.Sequence(elements.map(located), tags, nowhere)
      case Tree.Substitution(path, optional, tags, _) => Tree.Substitution(path, optional, tags, nowhere)
      case Tree.Injection(path, optional, tags, _, required) => Tree.Injection(path, optional, tags, nowhere, required)
      case Tree.Injections(items, tags, _) => Tree.Injections(items.map(_.map(reference)), tags, nowhere)
      case Tree.Concatenation(parts, tags, _) =>
        Tree.Concatenation(
          parts.map {
            case r: Reference => reference(r)
            case text: Text => text
          },
          tags,
          nowhere
        )
      case Tree.Layers(layers) => Tree.Layers(layers.map(layer => located(layer).asInstanceOf[Tree.Layer]))
      case written: Tree.Map =>
        // a block merged over what may turn out no map is kept as written: its statements are what it holds
        val map = written.steps.foldLeft(written.copy(steps = Vector.empty)) {
          case (map, Tree.Step.Statements(statements)) => new Merge().block(map, statements)
          case (_, step) => fail(step.toString)
        }
        Tree.Map(map.keys, map.entries.map { case (k, v) => k -> located(v) }, map.tag, nowhere)
      // no loader's marker in these trees
      case marked: Tree.Marked => fail(marked.toString)
    }

  private def reference(reference: Reference): Reference = reference.copy(location = nowhere)

  private def roundTrip(text: String): Unit = {
    val read = tree(text)
    located(tree(Render.document(read))) shouldBe located(read)
  }

  "Render" should "write a field per line, a map's fields in a block below its key, indented" in {
    Render.document(tree("a: 1\nb { c: x, d { } }\ne: null")) shouldBe "a: 1\nb:\n  c: x\n  d: {}\ne: null\n"
    Render.document(tree("")) shouldBe ""
  }

  it should "write a tag after a `:`, the block of a map below it, and the tag of every value" in {
    Render.document(tree("m: @z { k: @t v }\nn: @n null\no: @o []")) shouldBe
      "m: @z\n  k: @t v\nn: @n null\no: @o []\n"
  }

  it should "write a sequence as `- ` elements, the first line of a map or sequence on the line of its `- `" in {
    Render.document(
      tree("a:\n  - 1\n  - x\n  - null\n  - ${b}\n  ${c}\nc: [1, [2, 3], {d: 3, e: 4}, {}, [], @t {e: 4}, @u [5]]")) shouldBe
      "a:\n  - 1\n  - x\n  - null\n  - ${b}\n  ${c}\nc:\n  - 1\n  - - 2\n    - 3\n  - d: 3\n    e: 4\n  - {}\n" +
      "  - []\n  - @t\n    e: 4\n  - @u\n    - 5\n"
    Render.document(tree("a: [[[{b: 1, c: 2}]]]")) shouldBe "a:\n  - - - b: 1\n        c: 2\n"
  }

  it should "quote an element that would read as a field, or as a `- `" in {
    Render.document(tree("a: [\"x:\", \"y{\", \"b.c:=\", \"-\", \"x:y\", @t \"z:\"]")) shouldBe
      "a:\n  - \"x:\"\n  - \"y{\"\n  - \"b.c:=\"\n  - \"-\"\n  - x:y\n  - @t \"z:\"\n"
  }

  it should "quote keys and tags as Id.path does" in {
    Render.document(tree("`a b`: @`t:x` 1\n`1`.`` : 2")) shouldBe "`a b`: @`t:x` 1\n`1`:\n  ``: 2\n"
  }

  it should "write a scalar bare only when it reads back as itself" in {
    Render.value(Tree.Scalar("http://h:8080/x?y=1", None, nowhere)) shouldBe "http://h:8080/x?y=1"
    Render.value(Tree.Scalar("it's", None, nowhere)) shouldBe "it's"
    for (text <- List("", "null", "a b", "a,b", "a}", "'a'", "@a", "#a", "{a", "[a", "a\"b", "a`b", "//a", "a${b}"))
      Render.value(Tree.Scalar(text, None, nowhere)) should startWith("\"")
  }

  it should "escape in quotes what may not stand raw there, and a literal `${`" in {
    Render.value(Tree.Scalar("q\"b\\d$x${y}\b\f\n\r\t", None, nowhere)) shouldBe
      "\"q\\\"b\\\\d$x\\${y}\\b\\f\\n\\r\\t\""
    Render.value(Tree.Scalar("a\u0001\u200B\u0085\u2028\u2029\uD800z\uDC00", None, nowhere)) shouldBe
      "\"a\\u0001\\u200B\\u0085\\u2028\\u2029\\uD800z\\uDC00\""
    Render.value(Tree.Scalar("\uD83D\uDE00 \u00A0", None, nowhere)) shouldBe "\"\uD83D\uDE00 \u00A0\""
  }

  it should "quote a scalar with half a surrogate pair, which no encoding writes, escaping that half" in {
    Render.value(Tree.Scalar("\uD800", None, nowhere)) shouldBe "\"\\uD800\""
    Render.value(Tree.Scalar("a\uDC00", None, nowhere)) shouldBe "\"a\\uDC00\""
    Render.value(Tree.Scalar("a\uD83D\uDE00", None, nowhere)) shouldBe "a\uD83D\uDE00"
  }

  it should "write substitutions, and concatenations in quotes" in {
    Render.document(tree("a: ${b.c[0]}\nd: @t ${?e}\nf: x${g}y\nh: \"${i}\\\"\"")) shouldBe
      "a: ${b.c[0]}\nd: @t ${?e}\nf: \"x${g}y\"\nh: \"${i}\\\"\"\n"
  }

  it should "read back every example as the tree it was written from" in {
    roundTrip("a: 1\nb { c: x, d { } }\ne: null\nm: @z { k: @t v }\nc: [1, [2], {d: 3}, {}, @t {e: 4}]")
    roundTrip("a: [\"x:\", @t \"z:\", \"-\", [[{b: 1, c: [@t {d: 2}]}]], @u [], $x]")
    roundTrip("`a b`: @`t:x` \"q\\\"b\\\\d$x\\${y}\"\nf: x${g}y\nh: \"${i}\"\ns: \"\"")
  }

  private val strings: Gen[String] =
    Gen.oneOf(
      arbitrary[String],
      Gen.alphaNumStr,
      // ScalaCheck's own strings have no surrogates: whole range, and words with half a pair or both
      Gen.stringOf(Gen.choose(Char.MinValue, Char.MaxValue)),
      Gen.oneOf("null", "", "a b", "${x}", "\uD800", "a\uDC00", "\uDC00\uD800", "\uD83D\uDE00")
    )

  private val tagOptions: Gen[Option[String]] =
    Gen.frequency(3 -> Gen.const(None), 1 -> strings.map(Some(_)))

  private val path: Gen[Id.Branch] = {
    val step = Gen.oneOf(strings.map(Left(_)), Gen.choose(0, Int.MaxValue).map(Right(_)))
    for {
      first <- strings
      steps <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, step))
    } yield steps.foldLeft[Id.Branch](Id.Key(first, Id.Root)) {
      case (parent, Left(key)) => Id.Key(key, parent)
      case (parent, Right(index)) => Id.Index(index, parent)
    }
  }

  /** Parts as the lexer gives them: no empty text, no two texts in a row, a reference among them. */
  private val parts: Gen[Vector[Token.Part]] =
    Gen.listOf(Gen.oneOf(
      Gen.alphaNumStr.suchThat(_.nonEmpty).map(Text(_)),
      strings.suchThat(_.nonEmpty).map(Text(_)),
      path.map(Reference(_, optional = false, nowhere)),
      path.map(Reference(_, optional = true,
        nowhere))
    ))
      .map(_.foldLeft(Vector.empty[Token.Part]) {
        case (init :+ Text(a), Text(b)) => init :+ Text(a + b)
        case (all, part) => all :+ part
      })
      .suchThat(_.exists(_.isInstanceOf[Reference]))

  /** A value, `inBrackets` one that brackets can hold: no block of injections, and sequences that brackets write. */
  private def values(depth: Int, inBrackets: Boolean = false): Gen[Tree.Value] = {
    val leaves = Gen.oneOf(
      for {
        t <- strings
        g <- tagOptions
      } yield Tree.Scalar(t, g, nowhere),
      tagOptions.map(Tree.Null(_, nowhere)),
      for {
        p <- path
        o <- arbitrary[Boolean]
        g <- tagOptions
      } yield Tree.Substitution(p, o, g, nowhere),
      for {
        p <- parts
        g <- tagOptions
      } yield Tree.Concatenation(p, g, nowhere)
    )
    // a sequence of `- ` elements, an injection alone on its line among them, with one element at least
    val block = for {
      n <- Gen.choose(0, 4)
      es <- Gen.listOfN(n,
        Gen.frequency(4 -> values(depth - 1, inBrackets = false),
          1 -> references.map(r =>
            Tree.Injection(r.path, r.optional, None, nowhere, required = true))))
      g <- tagOptions
    } yield {
      val elements = if (es.nonEmpty && es.forall(_.isInstanceOf[Tree.Injection])) Tree.Null(None, nowhere) +: es
      else es
      Tree.Sequence(elements.toVector, g, nowhere)
    }
    // a sequence in brackets, an injection among its elements with its tags
    val bracketed = for {
      n <- Gen.choose(0, 4)
      es <- Gen.listOfN(
        n,
        Gen.frequency(4 -> values(depth - 1, inBrackets = true),
          1 ->
            (for {
              r <- references
              g <- tagOptions
            } yield Tree.Injection(r.path, r.optional, g, nowhere, required = false)))
      )
      g <- tagOptions
    } yield Tree.Sequence(es.toVector, g, nowhere)
    // in brackets a substitution is an injection, which stands among elements only
    val held = if (inBrackets) leaves.suchThat(!_.isInstanceOf[Tree.Substitution]) else leaves
    if (depth == 0) held
    else if (inBrackets) Gen.frequency(3 -> held, 1 -> maps(depth - 1), 1 -> bracketed)
    else Gen.frequency(3 -> leaves, 1 -> maps(depth - 1), 1 -> block, 1 -> bracketed, 1 -> injections)
  }

  private val references: Gen[Reference] =
    for {
      p <- path
      o <- arbitrary[Boolean]
    } yield Reference(p, o, nowhere)

  private val injections: Gen[Tree.Injections] =
    for {
      n <- Gen.choose(2, 3)
      rs <- Gen.listOfN(n, references)
      g <- tagOptions
    } yield Tree.Injections(rs.toVector.map(Right(_)), g, nowhere)

  /** What a key written again holds: a map, a substitution or a block of injections alone, no two maps in a row, which
    * would merge into one.
    */
  private def layers(depth: Int): Gen[Tree.Layers] = {
    val substitutions = for {
      r <- references
      g <- tagOptions
    } yield Tree.Substitution(r.path, r.optional, g, nowhere)
    val layer: Gen[Tree.Layer] = Gen.oneOf(substitutions, injections, maps(depth - 1))
    for {
      n <- Gen.choose(2, 3)
      ls <- Gen.listOfN(n, layer)
      fill <- substitutions
    } yield Tree.Layers(ls.foldLeft(Vector.empty[Tree.Layer]) {
      case (all @ (_ :+ (_: Tree.Map)), _: Tree.Map) => all :+ fill
      case (all, l) => all :+ l
    })
  }

  private def maps(depth: Int): Gen[Tree.Map] =
    for {
      n <- Gen.choose(0, 4)
      entries <- Gen.listOfN(
        n, Gen.zip(strings, if (depth == 0) values(0) else Gen.frequency(5 -> values(depth), 1 -> layers(depth))))
      g <- tagOptions
    } yield {
      val unique = entries.foldLeft(Vector.empty[(String, Tree.Value)]) { case (all, (k, v)) =>
        if (all.exists(_._1 == k)) all else all :+ (k -> v)
      }
      Tree.Map(unique.map(_._1), immutable.Map(unique*), g, nowhere)
    }

  it should "write any tree so that it parses and merges back to the same tree" in
    forAll(maps(3).map(_.copy(tag = None)), minSuccessful(1000)) { (root: Tree.Map) =>
      val written = Render.document(root)
      // through bytes, as a file is: what no encoding writes fails here, not in a user's file
      val bytes = StandardCharsets.UTF_8
        .newEncoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .encode(CharBuffer.wrap(written))
      val source = Source.decode("s", java.util.Arrays.copyOfRange(bytes.array, 0, bytes.limit))
      val parsed = Parser.parse(source.fold(e => fail(e.toString), identity))
      withClue(written) {
        parsed.errors shouldBe Nil
        val merged = Merge(::((parsed.value, nowhere), Nil))
        merged.errors shouldBe Nil
        located(merged.value) shouldBe located(root)
      }
    }

  it should "not write a block of injections in brackets, where none stands" in {
    val nowhere = SourceLocation("s", 1, 1)
    val reference = Reference(Id.Key("a", Id.Root), optional = false, nowhere)
    val sequence = Tree.Sequence(
      Vector(
        Tree.Injection(reference.path, optional = false, None, nowhere, required = false),
        Tree.Injections(Vector(Right(reference), Right(reference)), None, nowhere)
      ),
      None,
      nowhere
    )
    the[IllegalStateException] thrownBy Render.value(sequence) should have message
      "A block of injections at s:1:1 on one line"
  }

  it should "not write layers alone, which a key written again for each writes" in {
    val nowhere = SourceLocation("s", 1, 1)
    val sub = Tree.Substitution(Id.Key("a", Id.Root), optional = false, None, nowhere)
    the[IllegalStateException] thrownBy Render.value(Tree.Layers(sub, sub)) should have message
      "Layers at s:1:1 with no key to write again"
  }

  it should "not write what a call gave in a block of injections, which has no source" in {
    val nowhere = SourceLocation("s", 1, 1)
    val gave = AST.Injected.Given(AST.Call("d", Vector.empty, nowhere), AST.Null(None, nowhere))
    the[IllegalStateException] thrownBy Render.value(Tree.Injections(Vector(Left(gave)), None, nowhere)) should
      have message "What '#d' at s:1:1 gave in a block of injections has no source"
  }

  "toString" should "write the root as a document and any other node alone, a block below its tags" in {
    val root = Psi.parse("s", "a { b: [1, {c: 2}] }\nd: @t {}\ne: @u { f: 3 }\ng: @v [4]")
      .fold(e => fail(e.toString), identity)
    root.toString shouldBe "a:\n  b:\n    - 1\n    - c: 2\nd: @t {}\ne: @u\n  f: 3\ng: @v\n  - 4\n"
    root("a").toString shouldBe "b:\n  - 1\n  - c: 2"
    root("a") match {
      case map: Node.IMap[?] => map("b").toString shouldBe "- 1\n- c: 2"
      case other => fail(other.toString)
    }
    root("d").toString shouldBe "@t {}"
    root("e").toString shouldBe "@u\n  f: 3"
    root("g").toString shouldBe "@v\n  - 4"
  }
}
