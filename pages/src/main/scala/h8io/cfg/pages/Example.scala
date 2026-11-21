package h8io.cfg.pages

import h8io.cfg.impl.psi.{Psi, PsiLoader}

/** What the docs' `scala mdoc` blocks share; not published, and not in the API docs. */
object Example {

  /** The loader the examples use: `Psi` with no system properties, so that a page does not show the machine it was
    * built on.
    */
  val defaultLoader: PsiLoader = Psi.builder.withSystemProperties(Map.empty).build.fold(e => sys.error(e.toString), identity)

  /** Prints `source` and what it loads as, a tree written back as Psi or the report of its errors, as Markdown. Three
    * double quotes, which a Scala string cannot hold, are written QQQ.
    */
  def psi(source: String, loader: PsiLoader = defaultLoader): Unit = {
    val text = source.replace("QQQ", "\"\"\"").trim
    val (verb, result) = loader.parse("app.psi", text).fold(e => ("reports", e.message), r => ("loads as", r.toString))
    println("```\n" + text + "\n```\n\n" + verb + "\n\n```\n" + result.trim + "\n```")
  }
}
