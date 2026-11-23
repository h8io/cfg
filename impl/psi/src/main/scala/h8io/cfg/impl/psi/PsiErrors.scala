package h8io.cfg.impl.psi

import h8io.cfg.CfgError

/** Every error a load reported, each source's by line and column. The loader reports as many as it can without
  * inventing any.
  */
final case class PsiErrors(errors: ::[PsiError]) extends CfgError {

  /** The errors as one report: a count, then the errors grouped by source in the order above, each source named on a
    * line of its own and each error under it as `line:column: message`, its hint on the next line.
    */
  def message: String = {
    val groups = errors.groupBy(_.location.source)
    val names = errors.map(_.location.source).distinct
    val lines = names.flatMap(name => Message.visible(name) :: groups(name).flatMap(line))
    (count(errors.size, "error") + " in " + count(names.size, "source") :: lines).mkString("\n")
  }

  override def toString: String = message

  private def line(error: PsiError): List[String] = {
    val head = "  " + error.location.line.toString + ':' + error.location.column.toString + ": "
    (head + error.message) :: error.hint.map(" " * head.length + "hint: " + _).toList
  }

  private def count(n: Int, noun: String): String = n.toString + ' ' + noun + (if (n == 1) "" else "s")
}
