package h8io.cfg.impl.psi

import h8io.cfg.Id
import h8io.cfg.impl.psi.Token.{KeyMode, Reference, Text, ValueMode}

/** Writes a merged tree as Psi source, which parses and merges back to the same tree, its locations aside.
  *
  * It writes indentation, as a source is usually written (`format.md` §4): a field takes a line, and a map or a
  * sequence that holds anything stands in a block below its key, indented by two spaces, a map as its fields and a
  * sequence as `- ` elements; braces are left for the empty `{}` and `[]`. Tags go after the `:`, or after the `- ` of
  * an element, with the block below them. An element that is a map has its first field on the line of its `- `, and a
  * sequence its first element, `- - x`. Keys and tags are written as `Id.path` writes keys, bare when they can be and
  * in backticks otherwise (`format.md` §3); tags in alphabetical order. A scalar is written bare when the lexer reads
  * it back as the same scalar, in double quotes otherwise; a concatenation always in double quotes.
  */
private[psi] object Render {

  /** The fields of `map`, one per line, as the top level of a source. */
  def document(map: Tree.Map): String = {
    val out = new StringBuilder
    fields(out, map, 0)
    out.result()
  }

  /** `value` as it stands alone: a map as its fields and a sequence as its elements, at the top level, a block below
    * its tags when it has any; anything else as written after a `:`.
    */
  def value(value: Tree.Value): String = {
    val out = new StringBuilder
    if (value.tags.isEmpty) contents(out, value, 0) else after(out, value, 0)
    out.result().stripPrefix(" ").stripSuffix("\n")
  }

  private def indent(out: StringBuilder, level: Int): Unit = {
    out ++= "  " * level
    ()
  }

  private def fields(out: StringBuilder, map: Tree.Map, level: Int): Unit =
    map.keys.foreach { key =>
      indent(out, level)
      val _ = out ++= name(key) += ':'
      after(out, map.entries(key), level)
    }

  /** A value after a `:` or a `- ` that stands at `level`, its tags first, and the line ended. */
  private def after(out: StringBuilder, value: Tree.Value, level: Int): Unit = {
    tags(out, value)
    if (block(value)) {
      val _ = out += '\n'
      contents(out, value, level + 1)
    } else {
      val _ = out += ' '
      oneLine(out, value)
      val _ = out += '\n'
    }
  }

  /** A map or a sequence with something in it, which stands in a block below its key. */
  private def block(value: Tree.Value): Boolean =
    value match {
      case map: Tree.Map => map.keys.nonEmpty
      case sequence: Tree.Sequence => sequence.elements.nonEmpty
      case _ => false
    }

  /** The lines of a block at `level`: the fields of a map, the elements of a sequence, or a value alone. */
  private def contents(out: StringBuilder, value: Tree.Value, level: Int): Unit =
    value match {
      case map: Tree.Map if map.keys.nonEmpty => fields(out, map, level)
      case sequence: Tree.Sequence if sequence.elements.nonEmpty =>
        sequence.elements.foreach { element =>
          indent(out, level)
          val _ = out += '-'
          dashed(out, element, level)
        }
      // only a value alone without tags, as `value` writes one
      case _ =>
        oneLine(out, value)
        val _ = out += '\n'
    }

  /** An element after its `- `, which stands at `level`: an untagged map or sequence starts on the line of the `- `,
    * the rest of it aligned under that start; anything else as after a `:`.
    */
  private def dashed(out: StringBuilder, element: Tree.Value, level: Int): Unit =
    element match {
      case _ if element.tags.isEmpty && block(element) =>
        val _ = out += ' '
        val written = new StringBuilder
        contents(written, element, level + 1)
        val _ = out ++= written.result().drop(2 * (level + 1))
      // a scalar that would read as a field after a `- ` is quoted
      case Tree.Scalar(text, _, _) if bare(text) && field(text) =>
        tags(out, element)
        val _ = out += ' '
        quoted(out, Vector(Text(text)))
        val _ = out += '\n'
      case _ => after(out, element, level)
    }

  /** Whether `text` alone reads as the start of a field, as `x:` or `x{` does. */
  private def field(text: String): Boolean = {
    val lexer = new Lexer(Source("", text))
    lexer.next(KeyMode).kind.isInstanceOf[Token.Name] && lexer.fieldAhead()
  }

  /** The tags of `value`, each after a space, as they go after a `:` or a `- `. */
  private def tags(out: StringBuilder, value: Tree.Value): Unit =
    value.tags.toVector.sorted.foreach(tag => out ++= " @" ++= name(tag))

  /** A value on one line: a scalar, `null`, a substitution, or an empty map or sequence. */
  private def oneLine(out: StringBuilder, value: Tree.Value): Unit = {
    value match {
      case Tree.Scalar(text, _, _) => if (bare(text)) out ++= text else quoted(out, Vector(Text(text)))
      case Tree.Null(_, _) => out ++= "null"
      case _: Tree.Map => out ++= "{}"
      case _: Tree.Sequence => out ++= "[]"
      case Tree.Substitution(path, optional, _, _) => reference(out, path, optional, injection = false)
      case Tree.Injection(path, optional, _) => reference(out, path, optional, injection = true)
      case Tree.Concatenation(parts, _, _) => quoted(out, parts)
      // the loader's markers run as the tree is resolved, and only a resolved tree is shown
      case marked: Tree.Marked =>
        throw new IllegalStateException("Unresolved markers at " + marked.location.description)
    }
    ()
  }

  /** A key or a tag, as `Id.path` writes a key. */
  private def name(name: String): String = Id.Key(name, Id.Root).path

  private def reference(out: StringBuilder, path: Id.Branch, optional: Boolean, injection: Boolean): Unit = {
    val _ = out ++= (if (optional) "${?" else "${") ++= Message.path(path) ++= (if (injection) "*}" else "}")
  }

  /** Whether `text` reads back alone, unquoted, as itself: the lexer's own answer, `null` being no string. Half a
    * surrogate pair the lexer takes, but no encoding writes it, so only an escape in quotes keeps it.
    */
  private def bare(text: String): Boolean =
    text != "null" && !text.indices.exists(unpaired(text, _)) && {
      val lexer = new Lexer(Source("", text))
      lexer.next(ValueMode).kind == Token.Scalar(Vector(Text(text)), quoted = false) &&
      lexer.next(ValueMode).kind == Token.End && lexer.errorCount == 0
    }

  /** Text and references in double quotes, the text escaped: what may not stand raw in quotes, and a literal `\${`. */
  private def quoted(out: StringBuilder, parts: Vector[Token.Part]): Unit = {
    out += '"'
    parts.foreach {
      case Text(text) => escape(out, text)
      case Reference(path, optional, _, _) => reference(out, path, optional, injection = false)
    }
    out += '"'
    ()
  }

  private def escape(out: StringBuilder, text: String): Unit =
    text.indices.foreach { i =>
      val c = text.charAt(i)
      c match {
        case '"' => out ++= "\\\""
        case '\\' => out ++= "\\\\"
        case '$' if i + 1 < text.length && text.charAt(i + 1) == '{' => out ++= "\\$"
        case '\b' => out ++= "\\b"
        case '\f' => out ++= "\\f"
        case '\n' => out ++= "\\n"
        case '\r' => out ++= "\\r"
        case '\t' => out ++= "\\t"
        case _ if forbidden(text, i) => out ++= "\\u%04X".format(c.toInt)
        case _ => out += c
      }
    }

  /** A character that may not stand raw even in quotes (`format.md` §3), or half a surrogate pair with no other half.
    */
  private def forbidden(text: String, i: Int): Boolean = {
    val c = text.charAt(i)
    Lexer.isControl(c) || Lexer.isInvisible(c) || c == '\u0085' || c == '\u2028' || c == '\u2029' ||
    unpaired(text, i)
  }

  /** Half a surrogate pair at `i` of `text` with no other half. */
  private def unpaired(text: String, i: Int): Boolean = {
    val c = text.charAt(i)
    Character.isHighSurrogate(c) && !(i + 1 < text.length && Character.isLowSurrogate(text.charAt(i + 1))) ||
    Character.isLowSurrogate(c) && !(i > 0 && Character.isHighSurrogate(text.charAt(i - 1)))
  }
}
