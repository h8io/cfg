package h8io.cfg.impl.psi

/** Pieces of the texts of errors (`format.md` §11). */
private[psi] object Message {

  /** How many code points of found text a message shows before cutting it. */
  def Limit: Int = 20

  /** `text` made visible and kept on one line, then cut with `…` past [[Limit]] code points. */
  def cut(text: String): String = {
    val shown = visible(text)
    if (shown.codePointCount(0, shown.length) <= Limit) shown
    else shown.substring(0, shown.offsetByCodePoints(0, Limit)) + "…"
  }

  /** `text` with every character that would break a line or not show written as an escape, as `Id.path` writes one in a
    * key: a control, format or line-breaking character, and any space but U+0020.
    */
  def visible(text: String): String =
    text.iterator.map {
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ' ' => " "
      case c =>
        Character.getType(c) match {
          case Character.CONTROL | Character.FORMAT | Character.LINE_SEPARATOR | Character.PARAGRAPH_SEPARATOR |
              Character.SPACE_SEPARATOR =>
            escape(c)
          case _ => c.toString
        }
    }.mkString

  /** A path as `Id.path` writes it, built a segment at a time: `Id.path` recurses, and a path written in a source may
    * have any number of segments.
    */
  def path(id: h8io.cfg.Id): String = {
    @scala.annotation.tailrec
    def segments(id: h8io.cfg.Id, below: List[h8io.cfg.Id.Branch]): List[h8io.cfg.Id.Branch] =
      id match {
        case branch: h8io.cfg.Id.Branch => segments(branch.parent, branch :: below)
        case _ => below
      }
    segments(id, Nil).iterator.zipWithIndex.map {
      case (h8io.cfg.Id.Key(key, _), i) => (if (i == 0) "" else ".") + h8io.cfg.Id.Key(key, h8io.cfg.Id.Root).path
      case (h8io.cfg.Id.Index(index, _), _) => "[" + index.toString + "]"
    }.mkString
  }

  /** A key as it is written: bare, or in backticks with its escapes. */
  def key(name: String): String = cut(h8io.cfg.Id.Key(name, h8io.cfg.Id.Root).path)

  /** `text` in single quotes, cut. */
  def quote(text: String): String = "'" + cut(text) + "'"

  /** A character by its code point and name: `U+00A0 NO-BREAK SPACE`. */
  def char(c: Char): String = {
    val code = "U+%04X".format(c.toInt)
    Option(Character.getName(c.toInt)).fold(code)(code + ' ' + _)
  }

  /** The escape that writes `c` in a string. */
  def escape(c: Char): String = "\\u%04X".format(c.toInt)

  /** Said by an error after which the rest of its source is not read. */
  def Fatal: String = "; the rest of this source is not read"

  /** A directive's name as written after `#`. */
  def directive(name: String): String = "'#" + h8io.cfg.Id.Key(name, h8io.cfg.Id.Root).path + "'"
}
