package h8io.cfg.impl.psi

import java.io.{ByteArrayOutputStream, File, FileNotFoundException, IOException, InputStream, OutputStream}
import java.net.{JarURLConnection, SocketTimeoutException, URI, URL, URLConnection}
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.Arrays
import java.util.jar.JarFile
import java.util.zip.ZipFile
import scala.util.control.NonFatal

/** The text of one source, with a byte order mark at its start already skipped, and the means to turn an offset into
  * its text into a location.
  *
  * @param name
  *   the source as it was named (`format.md` §10)
  * @param url
  *   where it was read from, none for a source given as text
  * @param key
  *   what tells it from another source when directives parse sources (§6): a URL's normalized URI, a file's real path,
  *   or the name of a source given as text
  */
private[psi] final class Source private (val name: String, val text: String, val url: Option[URL], val key: String) {

  /** The offset at which each line starts. A line ends at LF; the CR of a CRLF is not part of it, a lone CR does not
    * end it.
    */
  private val lineStarts: Array[Int] =
    (0 +: text.indices.iterator.filter(text.charAt(_) == '\n').map(_ + 1).toVector).toArray

  /** The offset of each surrogate pair, at its high half: what makes a column differ from a count of chars. Counting
    * the code points of a line for every token instead takes time quadratic in its length, once the text holds any
    * character beyond Latin-1.
    */
  private val pairs: Array[Int] =
    text.indices.iterator
      .filter(i =>
        Character.isHighSurrogate(text.charAt(i)) && i + 1 < text.length &&
          Character.isLowSurrogate(text.charAt(i + 1)))
      .toArray

  /** The location of `offset`: a 1-based line and a 1-based column counted in code points. */
  def location(offset: Int): SourceLocation = {
    val found = Arrays.binarySearch(lineStarts, offset)
    val line = if (found >= 0) found else -found - 2
    val start = lineStarts(line)
    // a pair is one code point when both its halves stand before `offset`, two chars otherwise; a line starts after an
    // LF, so no pair stands across its start
    SourceLocation(name, line + 1, offset - start - (pairsBefore(offset - 1) - pairsBefore(start)) + 1)
  }

  /** How many surrogate pairs start before `offset`. */
  private def pairsBefore(offset: Int): Int = {
    val found = Arrays.binarySearch(pairs, offset)
    if (found >= 0) found else -found - 1
  }
}

private[psi] object Source {
  private def ByteOrderMark: Char = '\uFEFF'

  def apply(name: String, text: String): Source =
    new Source(name, if (text.nonEmpty && text.charAt(0) == ByteOrderMark) text.substring(1) else text, None, name)

  /** How long reading a URL waits for a connection, in milliseconds (`format.md` §6). */
  def ConnectTimeout: Int = 10000

  /** How long reading a URL waits for its next bytes, in milliseconds (`format.md` §6). */
  def ReadTimeout: Int = 30000

  /** Reads `url` as UTF-8, under the load's `budget`: an unreadable source and one that is not UTF-8 are errors, and so
    * is one that keeps the connection or its next bytes waiting past the timeouts; nothing when the budget is gone, by
    * this read or one before it, which the budget holds as its error.
    */
  def read(
      name: String,
      url: URL,
      budget: ReadBudget,
      connectTimeout: Int = ConnectTimeout,
      readTimeout: Int = ReadTimeout): Either[PsiError, Option[Source]] =
    if (budget.exhausted) Right(None)
    else
      try
        decode(name, new Reading(name, budget, connectTimeout, readTimeout).bytes(url))
          .map(source => Some(new Source(name, source.text, Some(url), key(url))))
      catch {
        case _: ReadBudget.Exceeded => Right(None)
        // a wait the time left cut short, which ends when it is gone
        case e: SocketTimeoutException =>
          try {
            val _ = budget.timeout(name, readTimeout)
            Left(SourceUnreadable(name, e))
          } catch { case _: ReadBudget.Exceeded => Right(None) }
        case e: IOException => Left(SourceUnreadable(name, e))
      }

  /** A connection to `url`, not yet made, with the timeouts set: a URL connection has none of its own, and a server
    * that never answers would hold the load forever.
    */
  def connect(url: URL, connectTimeout: Int = ConnectTimeout, readTimeout: Int = ReadTimeout): URLConnection = {
    val connection = url.openConnection()
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection
  }

  /** Reading the source `name`, every byte counted against `budget` and every wait cut to the time it has left. A
    * connection takes its timeouts once, when it is made, so a wait already begun runs its course: the time may be gone
    * past by one wait, at most the read timeout.
    */
  private final class Reading(name: String, budget: ReadBudget, connectTimeout: Int, readTimeout: Int) {
    def bytes(url: URL): Array[Byte] =
      if (url.getProtocol == "jar") entry(url)
      else {
        val out = new ByteArrayOutputStream
        fetch(url, out)
        out.toByteArray
      }

    /** Copies what `url` gives to `out`. */
    private def fetch(url: URL, out: OutputStream): Unit = {
      val stream =
        connect(url, budget.timeout(name, connectTimeout), budget.timeout(name, readTimeout)).getInputStream
      try copy(stream, out)
      finally stream.close()
    }

    /** Copies `in` to `out` a buffer at a time, counting each and looking at the time before each wait. */
    private def copy(in: InputStream, out: OutputStream): Unit = {
      val buffer = new Array[Byte](8192)
      var n = in.read(buffer)
      while (n >= 0) {
        budget.count(name, n)
        out.write(buffer, 0, n)
        val _ = budget.timeout(name, readTimeout)
        n = in.read(buffer)
      }
    }

    /** The entry of a jar `url` names, read from the jar itself rather than through the connection a `jar:` URL opens:
      * that one keeps a jar it opened in the JVM's cache, and gives every later load what the jar held then, and
      * fetches a remote jar with no timeouts at all. A remote jar is fetched whole into a temporary file, as the JVM
      * does, every byte of it counted.
      */
    private def entry(url: URL): Array[Byte] = {
      // opening no connection: a `JarURLConnection` parses the URL, and connects only once asked to
      val connection = url.openConnection().asInstanceOf[JarURLConnection]
      val entry = Option(connection.getEntryName).getOrElse(throw new IOException(url.toString + " names no entry"))
      val archive = connection.getJarFileURL
      if (archive.getProtocol == "file") {
        val file =
          try path(archive).toFile
          catch { case e: IllegalArgumentException => throw new IOException(archive.toString + " is no path", e) }
        this.entry(file, entry, url)
      } else {
        val temporary = Files.createTempFile("psi", ".jar")
        try {
          val out = Files.newOutputStream(temporary)
          try fetch(archive, out)
          finally out.close()
          this.entry(temporary.toFile, entry, url)
        } finally Files.delete(temporary)
      }
    }

    /** The entry `entry` of the jar `file`; one not there is not found, as the JVM reports it. */
    private def entry(file: File, entry: String, url: URL): Array[Byte] = {
      val jar = open(file, url)
      try {
        val found =
          Option(jar.getEntry(entry)).getOrElse(throw new FileNotFoundException(url.toString + " is not found"))
        val stream = jar.getInputStream(found)
        val out = new ByteArrayOutputStream
        try copy(stream, out)
        finally stream.close()
        out.toByteArray
      } finally jar.close()
    }
  }

  /** The path of a jar's `file:` URL, as a `JarURLConnection` gives it: with the fragment of the `jar:` URL, which
    * names no part of the path. A `file:` URL with a host, `file://host/x`, is no path: an `IllegalArgumentException`.
    */
  private[psi] def path(archive: URL): Path = Paths.get(URI.create(archive.toString.takeWhile(_ != '#')))

  /** The jar `file` that the `jar:` URL `url` names, opened as the JVM opens it: as of the version the JVM runs when
    * the URL ends in `#runtime`, a multi-release jar giving the entry of that version, and as of its base otherwise.
    */
  private[psi] def open(file: File, url: URL): JarFile =
    new JarFile(
      file,
      true,
      ZipFile.OPEN_READ,
      if (url.getRef == "runtime") JarFile.runtimeVersion else JarFile.baseVersion)

  /** The key of a source read from `url`: its URI normalized, a file's real path; the URL as it is written when it is
    * no URI, or the file is no longer there.
    */
  def key(url: URL): String =
    try {
      val uri = url.toURI.normalize
      if (uri.getScheme == "file") Paths.get(uri).toRealPath().toString else uri.toString
    } catch { case NonFatal(_) => url.toString }

  /** Decodes `bytes` as UTF-8, pointing at the first byte that is not. */
  def decode(name: String, bytes: Array[Byte]): Either[PsiError, Source] = {
    val decoder = StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    // UTF-8 never decodes to more chars than it has bytes
    val decoded = CharBuffer.allocate(bytes.length)
    val result = decoder.decode(ByteBuffer.wrap(bytes), decoded, true)
    val source = Source(name, decoded.flip().toString)
    if (result.isError) Left(InvalidEncoding(source.location(source.text.length))) else Right(source)
  }
}
