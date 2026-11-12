package h8io.cfg.impl.psi

import h8io.cfg.Id
import h8io.cfg.impl.psi.Token.{Reference, Text}
import h8io.cfg.impl.psi.Tree.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable

class MergeTest extends AnyFlatSpec with Matchers {
  private def document(name: String, text: String): (Ast.Document, SourceLocation) = {
    val parsed = Parser.parse(Source(name, text))
    parsed.errors shouldBe Nil
    (parsed.value, SourceLocation(name, 1, 1))
  }

  private def merge(texts: String*): Result[Tree.Map] = {
    val documents = texts.toList.zipWithIndex.map { case (text, i) => document("s" + i.toString, text) }
    Merge(::(documents.head, documents.tail))
  }

  /** The root of sources that merge with no error. */
  private def root(texts: String*): Tree.Map = {
    val merged = merge(texts*)
    merged.errors shouldBe Nil
    merged.value
  }

  private def loc(line: Int, column: Int, source: String = "s0"): SourceLocation = SourceLocation(source, line, column)

  private def str(value: String, line: Int, column: Int, tags: String*): Scalar =
    Scalar(value, tags.toSet, loc(line, column))

  private def map(sites: SourceLocation*)(entries: (String, Value)*): Tree.Map = tagged()(sites*)(entries*)

  private def tagged(tags: String*)(sites: SourceLocation*)(entries: (String, Value)*): Tree.Map =
    Tree.Map(
      entries.map(_._1).toVector,
      immutable.Map(entries*),
      tags.toSet,
      sites.tail.toList match {
        case Nil => sites.head
        case earlier @ ::(_, _) => MergedLocation(sites.head, earlier)
      }
    )

  private def top(entries: (String, Value)*): Tree.Map = map(loc(1, 1))(entries*)

  "Merge" should "read a source with no duplicate keys as written, its keys in order" in {
    root("b: 1\na: null\nc: [x, {d: 2}]\ne {}") shouldBe
      top(
        "b" -> str("1", 1, 4),
        "a" -> Null(Set.empty, loc(2, 4)),
        "c" -> Sequence(Vector(str("x", 3, 5), map(loc(3, 8))("d" -> str("2", 3, 12))), Set.empty, loc(3, 4)),
        "e" -> map(loc(4, 3))()
      )
  }

  it should "merge maps at a duplicate key, every block in its sites, the latest first" in {
    val merged = root("a { x: 1 }\nb: 0\na: { y: 2, x: 3 }")
    merged shouldBe
      top("a" -> map(loc(3, 4), loc(1, 3))("x" -> str("3", 3, 15), "y" -> str("2", 3, 9)), "b" -> str("0", 2, 4))
    merged.entries("a").location shouldBe MergedLocation(loc(3, 4), ::(loc(1, 3), Nil))
    merged.location shouldBe loc(1, 1)
  }

  it should "replace anything but a map with the last value, keeping the key's place" in {
    root("a: 1\nb: [1]\na: 2\nb: [2, 3]") shouldBe
      top("a" -> str("2", 3, 4), "b" -> Sequence(Vector(str("2", 4, 5), str("3", 4, 8)), Set.empty, loc(4, 4)))
    root("a { x: 1 }\na: 2\nb: 3\nb { y: 4 }") shouldBe
      top("a" -> str("2", 2, 4), "b" -> map(loc(4, 3))("y" -> str("4", 4, 8)))
  }

  it should "take null as any value that is not a map" in {
    root("a { x: 1 }\na: null\nb: null\nb { y: 2 }") shouldBe
      top("a" -> Null(Set.empty, loc(2, 4)), "b" -> map(loc(4, 3))("y" -> str("2", 4, 8)))
  }

  it should "read a dotted key as nested blocks, each standing at its key" in {
    root("a.b.c: 1\na.b.d: 2\na { e: 3 }") shouldBe
      top(
        "a" -> map(loc(3, 3), loc(2, 1), loc(1, 1))(
          "b" -> map(loc(2, 3), loc(1, 3))("c" -> str("1", 1, 8), "d" -> str("2", 2, 8)),
          "e" -> str("3", 3, 8)))
  }

  "`:=`" should "replace whatever its key held, keeping the key's place" in {
    root("p { min: 1, max: 10 }\nq: 0\np := { max: 4 }") shouldBe
      top("p" -> map(loc(3, 6))("max" -> str("4", 3, 13)), "q" -> str("0", 2, 4))
    root("a: 1\na := 2\nb := 3") shouldBe top("a" -> str("2", 2, 6), "b" -> str("3", 3, 6))
  }

  it should "apply to the last key of a dotted key only" in {
    root("a { x: 1, b { y: 2 } }\na.b := { z: 3 }") shouldBe
      top(
        "a" -> map(loc(2, 1), loc(1, 3))(
          "x" -> str("1", 1, 8),
          "b" -> map(loc(2, 8))("z" -> str("3", 2, 13))))
  }

  it should "replace inside a block merged into an earlier one" in {
    root("a { b { x: 1 }, c: 2 }\na { b := { y: 3 } }") shouldBe
      top(
        "a" -> map(loc(2, 3), loc(1, 3))(
          "b" -> map(loc(2, 10))("y" -> str("3", 2, 15)),
          "c" -> str("2", 1, 20)))
  }

  "Tags" should "be united when maps merge" in {
    root("r: @o { a: 1 }\nr { b: 2 }\nr: @s { c: 3 }") shouldBe
      top(
        "r" -> tagged("o", "s")(loc(3, 7), loc(2, 3), loc(1, 7))(
          "a" -> str("1", 1, 12),
          "b" -> str("2", 2, 8),
          "c" -> str("3", 3, 12)))
  }

  it should "be only its own on a value that replaces another, a map replaced with `:=` included" in {
    root("a: @x 1\na: 2\nb: @x { c: 1 }\nb := @y { d: 2 }") shouldBe
      top("a" -> str("2", 2, 4), "b" -> tagged("y")(loc(4, 9))("d" -> str("2", 4, 14)))
  }

  "A substitution" should "be kept unresolved, alone or in a concatenation" in {
    root("a: @t ${b.c}\nd: x${?e}") shouldBe
      top(
        "a" -> Substitution(Id.Key("c", Id.Key("b", Id.Root)), optional = false, Set("t"), loc(1, 7)),
        "d" -> Concatenation(
          Vector(Text("x"), Reference(Id.Key("e", Id.Root), optional = true, loc(2, 5))),
          Set.empty,
          loc(2, 4))
      )
  }

  it should "be replaced with `:`, and replace, where neither side is a map or a substitution" in {
    root("a: ${x}\na: 5\nb: 5\nb: ${x}\nc: \"${x}\"\nc { }") shouldBe
      top(
        "a" -> str("5", 2, 4),
        "b" -> Substitution(Id.Key("x", Id.Root), optional = false, Set.empty, loc(4, 4)),
        "c" -> map(loc(6, 3))())
  }

  it should "not merge with a map or a substitution, the key dropped and `:=` replacing" in {
    merge("a: ${s}\nb { p: 1 }\nc: ${?s}\nd: 1\na { p: 1 }\nb: ${s}\nc: ${t}\nd := ${s}") shouldBe
      Result(
        top("d" -> Substitution(Id.Key("s", Id.Root), optional = false, Set.empty, loc(8, 6))),
        List(
          MergeWithSubstitution(loc(5, 3), loc(1, 4)),
          MergeWithSubstitution(loc(6, 4), loc(2, 3)),
          MergeWithSubstitution(loc(7, 4), loc(3, 4)))
      )
    root("a: ${s}\na := { p: 1 }") shouldBe top("a" -> map(loc(2, 6))("p" -> str("1", 2, 11)))
  }

  it should "not merge at any depth, nor with the blocks of a dotted key" in {
    merge("g { h: ${x}, i: 1 }\ng { h { j: 1 } }\nk: ${x}\nk.l: 1") shouldBe
      Result(
        top("g" -> map(loc(2, 3), loc(1, 3))("i" -> str("1", 1, 17))),
        List(MergeWithSubstitution(loc(2, 7), loc(1, 8)), MergeWithSubstitution(loc(4, 1), loc(3, 4))))
  }

  it should "not hide the conflicts inside a block that conflicts itself" in {
    merge("a: ${x}\na {\n  b: ${y}\n  b {}\n}\nc: ${x}\nc { d: ${y}, d.e: 1 }") shouldBe
      Result(
        top(),
        List(
          MergeWithSubstitution(loc(2, 3), loc(1, 4)),
          MergeWithSubstitution(loc(4, 5), loc(3, 6)),
          MergeWithSubstitution(loc(7, 3), loc(6, 4)),
          MergeWithSubstitution(loc(7, 14), loc(7, 8))
        )
      )
  }

  it should "report a merged map where it was defined, every site of it" in {
    merge("a { x: 1 }\na { y: 2 }\na: ${s}").errors shouldBe
      List(MergeWithSubstitution(loc(3, 4), MergedLocation(loc(2, 3), ::(loc(1, 3), Nil))))
  }

  "Sources" should "merge in order as one, a `:=` in a later one replacing what an earlier one held" in {
    val merged = root("pool { min: 1, max: 10, idle: 30s }\nport: 1", "pool := { max: 4 }\nport: 2\nhost: h")
    merged shouldBe
      map(loc(1, 1, "s1"), loc(1, 1))(
        "pool" -> map(loc(1, 9, "s1"))("max" -> Scalar("4", Set.empty, loc(1, 16, "s1"))),
        "port" -> Scalar("2", Set.empty, loc(2, 7, "s1")),
        "host" -> Scalar("h", Set.empty, loc(3, 7, "s1"))
      )
    merged.location shouldBe MergedLocation(loc(1, 1, "s1"), ::(loc(1, 1), Nil))
  }

  it should "merge maps across sources" in {
    root("a { x: 1 }", "a { y: 2 }", "") shouldBe
      map(loc(1, 1, "s2"), loc(1, 1, "s1"), loc(1, 1))(
        "a" -> map(loc(1, 3, "s1"), loc(1, 3))("x" -> str("1", 1, 8), "y" -> Scalar("2", Set.empty, loc(1, 8, "s1"))))
  }

  "A directive" should "be a bug of the loader when it reaches merging unexpanded" in {
    the[IllegalStateException] thrownBy merge("#x(1)") should have message "#x at s0:1:1 was never expanded"
    the[IllegalStateException] thrownBy merge("a: [#y]") should have message "#y at s0:1:5 was never expanded"
  }
}
