package h8io.cfg.impl.psi

import java.io.{File, FileNotFoundException, IOException}
import java.net.{JarURLConnection, URI, URL, URLConnection}
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
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

  /** Reads `url` as UTF-8; an unreadable source and one that is not UTF-8 are errors, and so is one that keeps the
    * connection or its next bytes waiting past the timeouts.
    */
  def read(name: String, url: URL): Either[PsiError, Source] = read(name, url, ConnectTimeout, ReadTimeout)

  private[psi] def read(name: String, url: URL, connectTimeout: Int, readTimeout: Int): Either[PsiError, Source] =
    try
      decode(name, bytes(url, connectTimeout, readTimeout))
        .map(source => new Source(name, source.text, Some(url), key(url)))
    catch { case e: IOException => Left(SourceUnreadable(name, e)) }

  private def bytes(url: URL, connectTimeout: Int, readTimeout: Int): Array[Byte] =
    if (url.getProtocol == "jar") entry(url, connectTimeout, readTimeout)
    else {
      val stream = connect(url, connectTimeout, readTimeout).getInputStream
      try stream.readAllBytes()
      finally stream.close()
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

  /** The entry of a jar `url` names, read from the jar itself rather than through the connection a `jar:` URL opens:
    * that one keeps a jar it opened in the JVM's cache, and gives every later load what the jar held then, and fetches
    * a remote jar with no timeouts at all. A remote jar is fetched whole into a temporary file, as the JVM does.
    */
  private def entry(url: URL, connectTimeout: Int, readTimeout: Int): Array[Byte] = {
    // opening no connection: a `JarURLConnection` parses the URL, and connects only once asked to
    val connection = url.openConnection().asInstanceOf[JarURLConnection]
    val name = Option(connection.getEntryName).getOrElse(throw new IOException(url.toString + " names no entry"))
    val archive = connection.getJarFileURL
    if (archive.getProtocol == "file") {
      val file =
        try path(archive).toFile
        catch { case e: IllegalArgumentException => throw new IOException(archive.toString + " is no path", e) }
      entry(file, name, url)
    } else {
      val temporary = Files.createTempFile("psi", ".jar")
      try {
        val stream = connect(archive, connectTimeout, readTimeout).getInputStream
        try { val _ = Files.copy(stream, temporary, StandardCopyOption.REPLACE_EXISTING) }
        finally stream.close()
        entry(temporary.toFile, name, url)
      } finally Files.delete(temporary)
    }
  }

  /** The entry `name` of the jar `file`; one not there is not found, as the JVM reports it. */
  private def entry(file: File, name: String, url: URL): Array[Byte] = {
    val jar = open(file, url)
    try {
      val entry = Option(jar.getEntry(name)).getOrElse(throw new FileNotFoundException(url.toString + " is not found"))
      val stream = jar.getInputStream(entry)
      try stream.readAllBytes()
      finally stream.close()
    } finally jar.close()
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
