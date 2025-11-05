package h8io.cfg.raw

import java.util.regex.Pattern
import scala.annotation.switch

sealed trait Id {
  def path: String
}

object Id {
  case object Root extends Id {
    def path: String = ""
  }

  final case class Key(key: String, parent: Id) extends Id {
    def path: String = if (parent == Id.Root) quote(key) else parent.path + '.' + quote(key)
  }

  final case class Index(index: Int, parent: Id) extends Id {
    @inline def fits(n: Int): Boolean = index >= 0 && index < n

    def path: String = parent.path + '[' + index + ']'
  }

  private val SafeKeyPattern = Pattern.compile("^[\\p{L}_][\\p{L}\\p{N}_-]*$")
  @inline private def quote(key: String): String =
    if (SafeKeyPattern.matcher(key).matches()) key
    else ((key foldLeft new StringBuilder(key.length + 1, "\"")) { (builder, char) =>
      (char: @switch) match {
        case '"' | '\\' => builder += '\\' += char
        case '\b' => builder += '\\' += 'b'
        case '\f' => builder += '\\' += 'f'
        case '\n' => builder += '\\' += 'n'
        case '\r' => builder += '\\' += 'r'
        case '\t' => builder += '\\' += 't'
        case ' ' => builder += ' '
        case _ =>
          import Character.*
          (getType(char): @switch) match {
            case FORMAT | CONTROL | LINE_SEPARATOR | PARAGRAPH_SEPARATOR | SPACE_SEPARATOR | SURROGATE =>
              builder ++= f"\\u${char.toInt}%04X"
            case _ => builder += char
          }
      }
    } += '"').result()
}
