package h8io.cfg.impl.defg

import org.scalatest.{EitherValues, Inside}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class SourceTest extends AnyFlatSpec with Matchers with EitherValues with Inside {
  "location" should "count lines and columns from 1" in {
    val source = Source("s", "ab\ncd")
    source.location(0) shouldBe SourceLocation("s", 1, 1)
    source.location(1) shouldBe SourceLocation("s", 1, 2)
    source.location(2) shouldBe SourceLocation("s", 1, 3)
    source.location(3) shouldBe SourceLocation("s", 2, 1)
    source.location(5) shouldBe SourceLocation("s", 2, 3)
  }

  it should "count a column per code point, a tab as one" in {
    val source = Source("s", "😀\tж:")
    source.location(2) shouldBe SourceLocation("s", 1, 2)
    source.location(3) shouldBe SourceLocation("s", 1, 3)
    source.location(4) shouldBe SourceLocation("s", 1, 4)
  }

  it should "end a line at a CRLF but not at a lone CR" in {
    val source = Source("s", "a\r\nb\rc")
    source.location(1) shouldBe SourceLocation("s", 1, 2)
    source.location(3) shouldBe SourceLocation("s", 2, 1)
    source.location(5) shouldBe SourceLocation("s", 2, 3)
  }

  "apply" should "skip a byte order mark at the start only" in {
    Source("s", "\uFEFFa").text shouldBe "a"
    Source("s", "a\uFEFF").text shouldBe "a\uFEFF"
    Source("s", "").text shouldBe ""
  }

  "decode" should "read UTF-8 and skip a byte order mark" in {
    Source.decode("s", "\uFEFFключ: 😀".getBytes(StandardCharsets.UTF_8)).value.text shouldBe "ключ: 😀"
    Source.decode("s", Array.emptyByteArray).value.text shouldBe ""
  }

  it should "point at the first byte that is not UTF-8" in {
    val bytes = "a: 1\nbж".getBytes(StandardCharsets.UTF_8) ++ Array(0xff.toByte) ++
      "c".getBytes(StandardCharsets.UTF_8)
    Source.decode("s", bytes).left.value shouldBe InvalidEncoding(SourceLocation("s", 2, 3))
  }

  "read" should "read a URL" in {
    val file = Files.createTempFile("source", ".def")
    try {
      Files.write(file, "a: 1".getBytes(StandardCharsets.UTF_8))
      val source = Source.read("app.def", file.toUri.toURL).value
      source.name shouldBe "app.def"
      source.text shouldBe "a: 1"
    } finally Files.delete(file)
  }

  it should "report a URL that cannot be read" in {
    val file = Files.createTempFile("source", ".def")
    Files.delete(file)
    inside(Source.read("gone.def", file.toUri.toURL).left.value) { case SourceUnreadable("gone.def", _: IOException) =>
    }
  }
}
