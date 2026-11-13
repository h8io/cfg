package h8io.cfg.impl.psi

/** Pieces of the texts of errors (`format.md` §11). */
private[psi] object Message {

  /** How many code points of found text a message shows before cutting it. */
  def Limit: Int = 20

  /** `text`, cut with `…` past [[Limit]] code points. */
  def cut(text: String): String =
    if (text.codePointCount(0, text.length) <= Limit) text
    else text.substring(0, text.offsetByCodePoints(0, Limit)) + "…"

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
