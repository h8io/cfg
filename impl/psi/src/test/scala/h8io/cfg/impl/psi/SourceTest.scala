package h8io.cfg.impl.psi

import org.scalatest.{EitherValues, Inside}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{FileNotFoundException, IOException}
import java.net.{InetAddress, ServerSocket, SocketTimeoutException, URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}

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

  it should "count a surrogate pair as one code point once both its halves stand before, and a lone half as one" in {
    val text = "😀a\n😀😀\nb\uD83D\n\uDE00c\uD83D"
    val source = Source("s", text)
    (0 to text.length).foreach { offset =>
      val start = text.lastIndexOf('\n', offset - 1) + 1
      source.location(offset).column shouldBe text.codePointCount(start, offset) + 1
    }
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
    val file = Files.createTempFile("source", ".psi")
    try {
      Files.write(file, "a: 1".getBytes(StandardCharsets.UTF_8))
      val source = Source.read("app.psi", file.toUri.toURL).value
      source.name shouldBe "app.psi"
      source.text shouldBe "a: 1"
    } finally Files.delete(file)
  }

  it should "report a URL that cannot be read" in {
    val file = Files.createTempFile("source", ".psi")
    Files.delete(file)
    inside(Source.read("gone.psi", file.toUri.toURL).left.value) { case SourceUnreadable("gone.psi", _: IOException) =>
    }
  }

  "connect" should "wait for a connection and for the bytes read no longer than the timeouts" in {
    val connection = Source.connect(URI.create("http://127.0.0.1/app.psi").toURL)
    connection.getConnectTimeout shouldBe Source.ConnectTimeout
    connection.getReadTimeout shouldBe Source.ReadTimeout
    Source.ConnectTimeout should be > 0
    Source.ReadTimeout should be > 0
  }

  private def jar(path: Path, entries: (String, String)*): Path = {
    val out = new ZipOutputStream(Files.newOutputStream(path))
    try
      for ((name, text) <- entries) {
        out.putNextEntry(new ZipEntry(name))
        out.write(text.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
      }
    finally out.close()
    path
  }

  private def entry(jar: Path, name: String): URL = URI.create("jar:" + jar.toUri.toString + "!/" + name).toURL

  it should "read a jar as it is now, not as an earlier read found it" in {
    val path = jar(Files.createTempDirectory("psi").resolve("c.jar"), "a.psi" -> "a: old")
    Source.read("a", entry(path, "a.psi")).value.text shouldBe "a: old"
    // a new file, as a build puts one in place: the JVM's cache keeps the one it opened
    val next = jar(path.resolveSibling("next.jar"), "a.psi" -> "a: newer")
    Files.move(next, path, StandardCopyOption.REPLACE_EXISTING)
    Source.read("a", entry(path, "a.psi")).value.text shouldBe "a: newer"
  }

  it should "read a multi-release jar as of the version the JVM runs with #runtime, as of its base without" in {
    val path = jar(
      Files.createTempDirectory("psi").resolve("c.jar"),
      "META-INF/MANIFEST.MF" -> "Manifest-Version: 1.0\r\nMulti-Release: true\r\n",
      "a.psi" -> "v: base",
      "META-INF/versions/9/a.psi" -> "v: nine"
    )
    Source.read("a", entry(path, "a.psi#runtime")).value.text shouldBe "v: nine"
    Source.read("a", entry(path, "a.psi")).value.text shouldBe "v: base"
  }

  it should "report an entry its jar does not have as not found, and a jar URL that names no entry as unreadable" in {
    val path = jar(Files.createTempDirectory("psi").resolve("c.jar"), "a.psi" -> "a: 1")
    inside(Source.read("b", entry(path, "b.psi")).left.value) { case SourceUnreadable("b", _: FileNotFoundException) =>
    }
    inside(Source.read("root", entry(path, "")).left.value) { case SourceUnreadable("root", cause) =>
      cause should not be a[FileNotFoundException]
      cause.getMessage should include("names no entry")
    }
  }

  it should "report a jar whose file URL is no path" in {
    val path = jar(Files.createTempDirectory("psi").resolve("c.jar"), "a.psi" -> "a: 1")
    val archive = path.toUri.toURL.toString + "?q"
    inside(Source.read("a", URI.create("jar:" + archive + "!/a.psi").toURL).left.value) {
      case SourceUnreadable("a", cause) =>
        cause.getMessage shouldBe archive + " is no path"
        cause.getCause shouldBe an[IllegalArgumentException]
    }
    // a host: Java 17 takes the URL and fails on the path, later versions refuse the URL itself
    inside(Source.read("a", URI.create("jar:file://host/c.jar!/a.psi").toURL).left.value) {
      case SourceUnreadable("a", _: IOException) =>
    }
  }

  it should "give up on a server that sends nothing past the read timeout, a remote jar included" in {
    val server = new ServerSocket(0, 50, InetAddress.getLoopbackAddress)
    // the connections are accepted into the backlog and never answered
    try {
      val base = "http://127.0.0.1:" + server.getLocalPort.toString + "/"
      for (url <- List(base + "a.psi", "jar:" + base + "c.jar!/a.psi")) {
        val started = System.nanoTime
        inside(Source.read("a", URI.create(url).toURL, 1000, 200).left.value) {
          case SourceUnreadable("a", _: SocketTimeoutException) =>
        }
        (System.nanoTime - started) should be < 10000000000L
      }
    } finally server.close()
  }
}
