package h8io.cfg.impl.psi

import h8io.cfg.Id
import h8io.cfg.impl.psi.Token.{Reference, Text, ValueMode}

/** Writes a merged tree as Psi source, which parses and merges back to the same tree, its locations aside.
  *
  * A field takes a line, nested maps indented by two spaces in the block form `key { … }`, or `key: @t { … }` with
  * tags, which the block form has no place for. A sequence stands on one line, `[a, b]`, unless it holds a container,
  * and then takes a line per element. Keys and tags are written as `Id.path` writes keys, bare when they can be and in
  * backticks otherwise (`format.md` §3); tags in alphabetical order. A scalar is written bare when the lexer reads it
  * back as the same scalar, in double quotes otherwise; a concatenation always in double quotes.
  */
private[psi] object Render {

  /** The fields of `map`, one per line, as the top level of a source. */
  def document(map: Tree.Map): String = {
    val out = new StringBuilder
    fields(out, map, 0)
    out.result()
  }

  /** `value` as written after a `:`, with its tags. */
  def value(value: Tree.Value): String = {
    val out = new StringBuilder
    this.value(out, value, 0)
    out.result()
  }

  private def indent(out: StringBuilder, level: Int): Unit = {
    out ++= "  " * level
    ()
  }

  private def fields(out: StringBuilder, map: Tree.Map, level: Int): Unit =
    map.keys.foreach { key =>
      indent(out, level)
      out ++= name(key)
      map.entries(key) match {
        case nested: Tree.Map if nested.tags.isEmpty =>
          out += ' '
          block(out, nested, level)
        case other =>
          out ++= ": "
          value(out, other, level)
      }
      out += '\n'
    }

  private def block(out: StringBuilder, map: Tree.Map, level: Int): Unit =
    if (map.keys.isEmpty) out ++= "{}"
    else {
      out ++= "{\n"
      fields(out, map, level + 1)
      indent(out, level)
      out += '}'
    }

  private def value(out: StringBuilder, value: Tree.Value, level: Int): Unit = {
    value.tags.toVector.sorted.foreach(tag => out += '@' ++= name(tag) += ' ')
    value match {
      case Tree.Scalar(text, _, _) => if (bare(text)) out ++= text else quoted(out, Vector(Text(text)))
      case Tree.Null(_, _) => out ++= "null"
      case map: Tree.Map => block(out, map, level)
      case Tree.Sequence(elements, _, _) =>
        if (elements.isEmpty) out ++= "[]"
        else if (elements.forall(flat)) {
          out += '['
          elements.zipWithIndex.foreach { case (element, i) =>
            if (i > 0) out ++= ", "
            this.value(out, element, level)
          }
          out += ']'
        } else {
          out ++= "[\n"
          elements.foreach { element =>
            indent(out, level + 1)
            this.value(out, element, level + 1)
            out += '\n'
          }
          indent(out, level)
          out += ']'
        }
      case Tree.Substitution(path, optional, _, _) => reference(out, path, optional)
      case Tree.Concatenation(parts, _, _) => quoted(out, parts)
      // the loader's markers run as the tree is resolved, and only a resolved tree is shown
      case marked: Tree.Marked =>
        throw new IllegalStateException("Unresolved markers at " + marked.location.description)
    }
    ()
  }

  private def flat(value: Tree.Value): Boolean =
    value match {
      case _: Tree.Map | _: Tree.Sequence => false
      case _ => true
    }

  /** A key or a tag, as `Id.path` writes a key. */
  private def name(name: String): String = Id.Key(name, Id.Root).path

  private def reference(out: StringBuilder, path: Id.Branch, optional: Boolean): Unit = {
    val _ = out ++= (if (optional) "${?" else "${") ++= Message.path(path) += '}'
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
      case Reference(path, optional, _) => reference(out, path, optional)
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
