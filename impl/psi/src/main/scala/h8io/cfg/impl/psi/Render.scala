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
  *
  * A merged tree holds what is still unresolved, which only a test or a debugger shows. An injection among `- `
  * elements and a block of injections alone are written on lines of their own; a sequence holding an injection that
  * came from `[…]` is written in brackets, an element a line, a map in it in braces. A sequence of nothing but
  * injections among `- ` elements, which only a call can give, has no source that reads back as it, and neither has
  * what a call gave in a block of injections alone.
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

  /** The fields of `map` at `level`, the first one's indent left out when it goes on the line of a `- `. */
  private def fields(out: StringBuilder, map: Tree.Map, level: Int, onDash: Boolean = false): Unit =
    map.keys.zipWithIndex.foreach { case (key, i) =>
      if (i > 0 || !onDash) indent(out, level)
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

  /** A map or a sequence with something in it, or a block of injections alone, which stands in a block below its key.
    */
  private def block(value: Tree.Value): Boolean =
    value match {
      case map: Tree.Map => map.keys.nonEmpty
      case sequence: Tree.Sequence => sequence.elements.nonEmpty
      case _: Tree.Injections => true
      case _ => false
    }

  /** The lines of a block at `level`: the fields of a map, the elements of a sequence, or a value alone; the first
    * line's indent left out when it goes on the line of a `- `.
    */
  private def contents(out: StringBuilder, value: Tree.Value, level: Int, onDash: Boolean = false): Unit =
    value match {
      case map: Tree.Map if map.keys.nonEmpty => fields(out, map, level, onDash)
      case sequence: Tree.Sequence if sequence.elements.exists(spread) =>
        if (!onDash) indent(out, level)
        bracketed(out, sequence, level)
        val _ = out += '\n'
      case sequence: Tree.Sequence if sequence.elements.nonEmpty =>
        sequence.elements.zipWithIndex.foreach { case (element, i) =>
          if (i > 0 || !onDash) indent(out, level)
          element match {
            // alone on its line, it puts the elements of a sequence
            case _: Tree.Injection =>
              oneLine(out, element)
              val _ = out += '\n'
            case _ =>
              val _ = out += '-'
              dashed(out, element, level)
          }
        }
      case Tree.Injections(items, _, _) =>
        items.zipWithIndex.foreach { case (item, i) =>
          if (i > 0 || !onDash) indent(out, level)
          item.fold(
            gave =>
              throw new IllegalStateException(
                "What " + Message.directive(gave.call.name) + " at " + gave.call.location.description +
                  " gave in a block of injections has no source"),
            r => reference(out, r.path, r.optional)
          )
          val _ = out += '\n'
        }
      // only a value alone without tags, as `value` writes one
      case _ =>
        oneLine(out, value)
        val _ = out += '\n'
    }

  /** Whether `element` is an injection from `[…]`, which only brackets write. */
  private def spread(element: Tree.Value): Boolean =
    element match {
      case injection: Tree.Injection => !injection.required
      case _ => false
    }

  /** A sequence in brackets that stands at `level`: `[`, an element a line below it, one level deeper, a map in braces
    * and a sequence in brackets, and `]` on a line of its own.
    */
  private def bracketed(out: StringBuilder, sequence: Tree.Sequence, level: Int): Unit = {
    out ++= "[\n"
    sequence.elements.foreach { element =>
      indent(out, level + 1)
      element.tags.toVector.sorted.foreach(tag => out ++= "@" ++= name(tag) += ' ')
      element match {
        case map: Tree.Map if map.keys.nonEmpty =>
          out ++= "{\n"
          fields(out, map, level + 2)
          indent(out, level + 1)
          out += '}'
        case sequence: Tree.Sequence if sequence.elements.nonEmpty => bracketed(out, sequence, level + 1)
        case _ => oneLine(out, element)
      }
      out += '\n'
    }
    indent(out, level)
    out += ']'
    ()
  }

  /** An element after its `- `, which stands at `level`: an untagged map or sequence starts on the line of the `- `,
    * the rest of it aligned under that start; anything else as after a `:`.
    */
  private def dashed(out: StringBuilder, element: Tree.Value, level: Int): Unit =
    element match {
      case _ if element.tags.isEmpty && block(element) =>
        val _ = out += ' '
        contents(out, element, level + 1, onDash = true)
      // a scalar that would read as a field after a `- ` is quoted
      case Tree.Scalar(text, _, _) if bare(text) && field(text) =>
        tags(out, element)
        val _ = out += ' '
        quoted(out, Vector(Text(text)))
        val _ = out += '\n'
      case _ => after(out, element, level)
    }

  /** Whether `text` alone reads as the start of a field, as `x:` or `x{` does: only text with a `:` or a `{` can. */
  private def field(text: String): Boolean = (text.indexOf(':') >= 0 || text.indexOf('{') >= 0) && {
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
      case Tree.Substitution(path, optional, _, _) => reference(out, path, optional)
      case Tree.Injection(path, optional, _, _, _) => reference(out, path, optional)
      case injections: Tree.Injections =>
        throw new IllegalStateException("A block of injections at " + injections.location.description + " on one line")
      case Tree.Concatenation(parts, _, _) => quoted(out, parts)
      // the loader's markers run as the tree is resolved, and only a resolved tree is shown
      case marked: Tree.Marked =>
        throw new IllegalStateException("Unresolved markers at " + marked.location.description)
    }
    ()
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
