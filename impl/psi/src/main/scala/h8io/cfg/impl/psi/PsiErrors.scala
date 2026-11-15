package h8io.cfg.impl.psi

import h8io.cfg.CfgError

/** Every error a load reported, in the order found. The loader reports as many as it can without inventing any. */
final case class PsiErrors(errors: ::[PsiError]) extends CfgError {

  /** The errors as one report (`format.md` §11): a count, then the errors grouped by source in the order found, each
    * source named on a line of its own and each error under it as `line:column: message`, its hint on the next line. An
    * error at a location in no source is grouped under that location's description.
    */
  def message: String = {
    val groups = errors.groupBy(error => group(error.location))
    val names = errors.map(error => group(error.location)).distinct
    val lines = names.flatMap(name => Message.visible(name) :: groups(name).flatMap(line))
    (count(errors.size, "error") + " in " + count(names.size, "source") :: lines).mkString("\n")
  }

  override def toString: String = message

  private def line(error: PsiError): List[String] = {
    val head = "  " + error.location.anchor.fold("")(at => at.line.toString + ':' + at.column.toString + ": ")
    (head + error.message + origin(error.location)) :: error.hint.map(" " * head.length + "hint: " + _).toList
  }

  private def group(location: PsiLocation): String = location.anchor.fold(location.description)(_.source)

  /** What a composite location says past its anchor, such as the origin of a substituted value. */
  private def origin(location: PsiLocation): String =
    location.anchor.fold("")(at => location.description.stripPrefix(at.description))

  private def count(n: Int, noun: String): String = n.toString + ' ' + noun + (if (n == 1) "" else "s")
}
