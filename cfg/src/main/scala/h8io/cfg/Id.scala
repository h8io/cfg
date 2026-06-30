package h8io.cfg

import java.util.regex.Pattern
import scala.annotation.switch

/** A path addressing a position in the config tree.
  *
  * Every [[Node]] carries an `Id` that identifies where in the tree it lives. The [[path]] method renders the full
  * address as a human-readable string using dotted-key notation with bracket-indexed sequences, e.g. `server.hosts[0]`.
  *
  * Concrete implementations:
  *   - [[Id.Root]] — the top of the tree; [[path]] is the empty string
  *   - [[Id.Key]] — a named field inside a map node
  *   - [[Id.Index]] — a positional element inside a sequence node
  */
sealed trait Id {
  def path: String
}

object Id {

  /** The root of the config tree. Its [[path]] is the empty string. */
  sealed trait Root extends Id {
    final def path: String = ""
  }

  case object Root extends Root

  /** A non-root path segment that has a [[parent]]. */
  sealed trait Branch extends Id {
    def parent: Id
  }

  /** A named field inside a map node.
    *
    * Keys that contain characters outside `[A-Za-z_][A-Za-z0-9_-]*` are quoted and escaped in [[path]] so the output is
    * always unambiguous.
    *
    * @param key
    *   the field name
    * @param parent
    *   the parent path
    */
  final case class Key(key: String, parent: Id) extends Branch {
    def path: String = if (parent == Id.Root) quote(key) else parent.path + '.' + quote(key)
  }

  /** A positional element inside a sequence node.
    *
    * @param index
    *   zero-based position
    * @param parent
    *   the parent path
    */
  final case class Index(index: Int, parent: Id) extends Branch {

    /** Returns `true` if this index is within `[0, n)`. */
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
