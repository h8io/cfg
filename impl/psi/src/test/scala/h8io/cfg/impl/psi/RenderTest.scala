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
      case Tree.Concatenation(parts, tags, _) =>
        Tree.Concatenation(
          parts.map {
            case Reference(path, optional, _) => Reference(path, optional, nowhere)
            case text: Text => text
          },
          tags,
          nowhere)
      case map: Tree.Map =>
        Tree.Map(map.keys, map.entries.map { case (k, v) => k -> located(v) }, map.tags, nowhere)
      // no loader's marker in these trees
      case marked: Tree.Marked => fail(marked.toString)
    }

  private def roundTrip(text: String): Unit = {
    val read = tree(text)
    located(tree(Render.document(read))) shouldBe located(read)
  }

  "Render" should "write a field per line, nested maps in the block form, indented" in {
    Render.document(tree("a: 1\nb { c: x, d { } }\ne: null")) shouldBe "a: 1\nb {\n  c: x\n  d {}\n}\ne: null\n"
    Render.document(tree("")) shouldBe ""
  }

  it should "write a tagged map after a `:`, tags sorted, and tags of every value" in {
    Render.document(tree("m: @z @a { k: @t v }\nn: @n null\no: @o []")) shouldBe
      "m: @a @z {\n  k: @t v\n}\nn: @n null\no: @o []\n"
  }

  it should "write a sequence on one line unless it holds a container" in {
    Render.document(tree("a: [1, x, null, ${b}]\nc: [1, [2], {d: 3}, {}, @t {e: 4}]")) shouldBe
      "a: [1, x, null, ${b}]\nc: [\n  1\n  [2]\n  {\n    d: 3\n  }\n  {}\n  @t {\n    e: 4\n  }\n]\n"
  }

  it should "quote keys and tags as Id.path does" in {
    Render.document(tree("`a b`: @`t:x` 1\n`1`.`` : 2")) shouldBe "`a b`: @`t:x` 1\n`1` {\n  ``: 2\n}\n"
  }

  it should "write a scalar bare only when it reads back as itself" in {
    Render.value(Tree.Scalar("http://h:8080/x?y=1", Set.empty, nowhere)) shouldBe "http://h:8080/x?y=1"
    Render.value(Tree.Scalar("it's", Set.empty, nowhere)) shouldBe "it's"
    for (text <- List("", "null", "a b", "a,b", "a}", "'a'", "@a", "#a", "{a", "[a", "a\"b", "a`b", "//a", "a${b}"))
      Render.value(Tree.Scalar(text, Set.empty, nowhere)) should startWith("\"")
  }

  it should "escape in quotes what may not stand raw there, and a literal `${`" in {
    Render.value(Tree.Scalar("q\"b\\d$x${y}\b\f\n\r\t", Set.empty, nowhere)) shouldBe
      "\"q\\\"b\\\\d$x\\${y}\\b\\f\\n\\r\\t\""
    Render.value(Tree.Scalar("a\u0001\u200B\u0085\u2028\u2029\uD800z\uDC00", Set.empty, nowhere)) shouldBe
      "\"a\\u0001\\u200B\\u0085\\u2028\\u2029\\uD800z\\uDC00\""
    Render.value(Tree.Scalar("\uD83D\uDE00 \u00A0", Set.empty, nowhere)) shouldBe "\"\uD83D\uDE00 \u00A0\""
  }

  it should "quote a scalar with half a surrogate pair, which no encoding writes, escaping that half" in {
    Render.value(Tree.Scalar("\uD800", Set.empty, nowhere)) shouldBe "\"\\uD800\""
    Render.value(Tree.Scalar("a\uDC00", Set.empty, nowhere)) shouldBe "\"a\\uDC00\""
    Render.value(Tree.Scalar("a\uD83D\uDE00", Set.empty, nowhere)) shouldBe "a\uD83D\uDE00"
  }

  it should "write substitutions, and concatenations in quotes" in {
    Render.document(tree("a: ${b.c[0]}\nd: @t ${?e}\nf: x${g}y\nh: \"${i}\\\"\"")) shouldBe
      "a: ${b.c[0]}\nd: @t ${?e}\nf: \"x${g}y\"\nh: \"${i}\\\"\"\n"
  }

  it should "read back every example as the tree it was written from" in {
    roundTrip("a: 1\nb { c: x, d { } }\ne: null\nm: @z @a { k: @t v }\nc: [1, [2], {d: 3}, {}, @t {e: 4}]")
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

  private val tagSets: Gen[Set[String]] =
    Gen.frequency(3 -> Gen.const(Set.empty[String]), 1 -> Gen.listOf(strings).map(_.toSet))

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

  private def values(depth: Int): Gen[Tree.Value] = {
    val leaves = Gen.oneOf(
      for {
        t <- strings
        g <- tagSets
      } yield Tree.Scalar(t, g, nowhere),
      tagSets.map(Tree.Null(_, nowhere)),
      for {
        p <- path
        o <- arbitrary[Boolean]
        g <- tagSets
      } yield Tree.Substitution(p, o, g, nowhere),
      for {
        p <- parts
        g <- tagSets
      } yield Tree.Concatenation(p, g, nowhere)
    )
    if (depth == 0) leaves
    else
      Gen.frequency(
        3 -> leaves,
        1 -> maps(depth - 1),
        1 ->
          (for {
            n <- Gen.choose(0, 4)
            es <- Gen.listOfN(n, values(depth - 1))
            g <- tagSets
          } yield Tree.Sequence(es.toVector, g, nowhere))
      )
  }

  private def maps(depth: Int): Gen[Tree.Map] =
    for {
      n <- Gen.choose(0, 4)
      entries <- Gen.listOfN(n, Gen.zip(strings, values(depth)))
      g <- tagSets
    } yield {
      val unique = entries.foldLeft(Vector.empty[(String, Tree.Value)]) { case (all, (k, v)) =>
        if (all.exists(_._1 == k)) all else all :+ (k -> v)
      }
      Tree.Map(unique.map(_._1), immutable.Map(unique*), g, nowhere)
    }

  it should "write any tree so that it parses and merges back to the same tree" in
    forAll(maps(3).map(_.copy(tags = Set.empty)), minSuccessful(1000)) { (root: Tree.Map) =>
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

  "toString" should "write the root as a document and any other node as a value" in {
    val root = Psi.parse("s", "a { b: [1, {c: 2}] }\nd: @t {}").fold(e => fail(e.toString), identity)
    root.toString shouldBe "a {\n  b: [\n    1\n    {\n      c: 2\n    }\n  ]\n}\nd: @t {}\n"
    root("a").toString shouldBe "{\n  b: [\n    1\n    {\n      c: 2\n    }\n  ]\n}"
    root("a") match {
      case map: Node.IMap[?] => map("b").toString shouldBe "[\n  1\n  {\n    c: 2\n  }\n]"
      case other => fail(other.toString)
    }
  }
}
