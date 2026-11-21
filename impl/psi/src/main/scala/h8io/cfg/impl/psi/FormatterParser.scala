package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Lexer.*
import h8io.cfg.impl.psi.MarkerParser.Call

/** Reads the formatter of a substitution (`format.md` §9) from just after its `;`: whitespace, a call written as a
  * marker's is, its name by the key rule, and the `}` that closes the substitution. `line` is the rest of the source's
  * line, up to a forbidden character, and `offsets` the offset in the source of each of its characters and of the end
  * after them; a `"` or a `}` inside an argument's literal closes nothing.
  *
  * The call is bound to its arguments as it is read, the built-in `join` or a loader's formatter, whose handler is
  * called with them.
  */
private[psi] final class FormatterParser(
    text: CharSequence,
    positions: IndexedSeq[Int],
    reporter: (SourceLocation => PsiError, Int) => Unit,
    locator: Int => SourceLocation,
    formatters: Map[String, Formatter])
    extends CallReader(text, positions, reporter, locator) {

  protected val invalidCall: SourceLocation => PsiError = InvalidFormatter.apply

  /** The formatter bound to its arguments, or none once an error has been reported; and where reading stopped in
    * `line`: at the `}` when there is a formatter.
    */
  def formatter(): (Option[Token.Formatting], Int) = {
    skipSpaces()
    val start = i
    val read = name().flatMap(name =>
      if (at(i, '(')) arguments().map(Call(name, start, _)) else Some(Call(name, start, Vector.empty)))
    val end = i
    read.foreach(_ => closed())
    (read.filter(_ => !failed).flatMap(bound(_, line.subSequence(start, end).toString)), i)
  }

  /** A name by the key rule, bare or in backticks. */
  private def name(): Option[String] =
    if (at(i, '`')) backticked()
    else {
      val start = i
      identifier()
      if (i > start) Some(line.subSequence(start, i).toString) else invalid
    }

  /** The `}` after the call; whitespace, a second formatter or anything else before it reported. */
  private def closed(): Unit =
    if (!at(i, '}'))
      if (has(i) && isSpace(line.charAt(i))) error(WhitespaceInSubstitution.apply, i)
      else if (at(i, ';')) error(SecondFormatter.apply, i)
      else syntaxError()

  /** `call`, written `written`, bound to its arguments; none once an error has been reported. */
  private def bound(call: Call, written: String): Option[Token.Formatting] = {
    val location = locate(offsets(call.at))
    if (call.name == "join") {
      val separator = bind(call, Vector(Parameter("separator", required = true))).get("separator")
      separator.filter(_ => !failed).map(argument => Token.Join(text(argument.value), written, location))
    } else
      formatters.get(call.name) match {
        case Some(formatter) =>
          val arguments = handed(call, bind(call, formatter.parameters.toVector))
          if (failed) None
          else
            formatter(arguments) match {
              case Right(format) => Some(Token.Handled(call.name, written, location, format))
              case Left(problem) =>
                error(FormatterFailed(_, call.name, problem), call.at)
                None
            }
        case None =>
          error(UnknownFormatter(_, call.name), call.at)
          None
      }
  }
}
