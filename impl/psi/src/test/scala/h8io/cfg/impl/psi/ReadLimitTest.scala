package h8io.cfg.impl.psi

import h8io.cfg.{Id, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetAddress, ServerSocket, URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Duration
import java.util.zip.{ZipEntry, ZipOutputStream}

class ReadLimitTest extends AnyFlatSpec with Matchers {
  private def loader(bytes: Int, time: Duration = Duration.ofSeconds(60)): PsiLoader =
    Psi.builder
      .withSystemProperties(Map.empty)
      .withReadLimit(bytes, time)
      .build
      .fold(e => fail(e.toString), identity)

  private def write(dir: Path, name: String, text: String): Path =
    Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))

  private def url(path: Path): URL = path.toUri.toURL

  private def errors(result: Either[PsiErrors, Node.IMap[Id.Root]]): List[PsiError] =
    result.fold(_.errors, root => fail(root.toString))

  "The read limit" should "be at least 0 bytes and a time not negative" in {
    Psi.builder.withReadLimit(0, Duration.ZERO).build.isRight shouldBe true
    for ((bytes, time) <- List((-1, Duration.ZERO), (0, Duration.ofMillis(-1)), (-1, Duration.ofMillis(-1))))
      Psi.builder.withReadLimit(bytes, time).build shouldBe
        Left(PsiBuildErrors(::(InvalidReadLimit(bytes, time), Nil)))
  }

  it should "not count a source given as text" in {
    loader(0, Duration.ZERO).parse("s", "a: 1").map(_.size) shouldBe Right(1)
  }

  "Reading" should "take as many bytes as the limit, and no more, the source that goes past reported alone" in {
    val dir = Files.createTempDirectory("psi")
    val (a, b) = (write(dir, "a.psi", "a: 1"), write(dir, "b.psi", "b: 2"))
    val missing = dir.resolve("missing.psi")
    loader(8)(url(a), url(b)).map(_.size) shouldBe Right(2)
    // what is read after the limit is dropped, not reported, a source not there among them
    errors(loader(7)(url(a), url(b), url(missing))) shouldBe List(ReadLimitExceeded(url(b).toString, 7))
  }

  it should "count a source as often as it is read" in {
    val dir = Files.createTempDirectory("psi")
    val common = write(dir, "common.psi", "x: 1")
    val main = write(dir, "main.psi", "a: #include(file: \"common\")\nb: #include(file: \"common\")")
    val size = Files.size(main).toInt
    loader(size + 8)(url(main)).map(_.size) shouldBe Right(2)
    errors(loader(size + 7)(url(main))) shouldBe List(ReadLimitExceeded(url(common).toString, size + 7))
  }

  it should "count the entry of a jar it reads, and the whole of a jar it fetches" in {
    val dir = Files.createTempDirectory("psi")
    val jar = dir.resolve("c.jar")
    val out = new ZipOutputStream(Files.newOutputStream(jar))
    try {
      out.putNextEntry(new ZipEntry("a.psi"))
      out.write("a: 1".getBytes(StandardCharsets.UTF_8))
      out.closeEntry()
    } finally out.close()
    val entry = URI.create("jar:" + url(jar).toString + "!/a.psi").toURL
    loader(4)(entry).map(_.size) shouldBe Right(1)
    errors(loader(3)(entry)) shouldBe List(ReadLimitExceeded(entry.toString, 3))
  }

  "Includes of includes, ten each and six deep" should "go past the default limit, not run out of memory" in {
    val dir = Files.createTempDirectory("psi")
    write(dir, "f0.psi", "x: 1")
    for (i <- 1 to 6)
      write(
        dir,
        "f" + i.toString + ".psi",
        (0 until 10).map(k => "k" + k.toString + ": #include(file: \"f" + (i - 1).toString + "\")").mkString("\n"))
    errors(Psi(url(dir.resolve("f6.psi")))).map(_.getClass) shouldBe List(classOf[ReadLimitExceeded])
  }

  "A load" should "stop reading once its time is gone, the source being read reported alone" in {
    val dir = Files.createTempDirectory("psi")
    val a = write(dir, "a.psi", "a: 1")
    errors(loader(100, Duration.ZERO)(url(a), url(a))) shouldBe List(ReadTimeExceeded(url(a).toString, Duration.ZERO))
    // a server that takes the connection and never answers, its wait cut to the time left
    val server = new ServerSocket(0, 50, InetAddress.getLoopbackAddress)
    try {
      val base = "http://127.0.0.1:" + server.getLocalPort.toString + "/"
      for (target <- List(base + "a.psi", "jar:" + base + "c.jar!/a.psi")) {
        val time = Duration.ofMillis(300)
        val started = System.nanoTime
        errors(loader(100, time)(URI.create(target).toURL)) shouldBe List(ReadTimeExceeded(target, time))
        (System.nanoTime - started) should be < 10000000000L
      }
    } finally server.close()
  }

  it should "take a time too long for nanoseconds as forever" in {
    val a = write(Files.createTempDirectory("psi"), "a.psi", "a: 1")
    loader(100, Duration.ofSeconds(Long.MaxValue))(url(a)).map(_.size) shouldBe Right(1)
  }
}
