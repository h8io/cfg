package h8io.cfg.impl.psi

import java.io.IOException
import java.net.URL
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.Paths
import java.nio.{ByteBuffer, CharBuffer}
import java.util.Arrays
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

  /** The location of `offset`: a 1-based line and a 1-based column counted in code points. */
  def location(offset: Int): SourceLocation = {
    val found = Arrays.binarySearch(lineStarts, offset)
    val line = if (found >= 0) found else -found - 2
    SourceLocation(name, line + 1, text.codePointCount(lineStarts(line), offset) + 1)
  }
}

private[psi] object Source {
  private def ByteOrderMark: Char = '\uFEFF'

  def apply(name: String, text: String): Source =
    new Source(name, if (text.nonEmpty && text.charAt(0) == ByteOrderMark) text.substring(1) else text, None, name)

  /** Reads `url` as UTF-8; an unreadable source and one that is not UTF-8 are errors. */
  def read(name: String, url: URL): Either[PsiError, Source] =
    try {
      val stream = url.openStream()
      try decode(name, stream.readAllBytes()).map(source => new Source(name, source.text, Some(url), key(url)))
      finally stream.close()
    } catch { case e: IOException => Left(SourceUnreadable(name, e)) }

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
