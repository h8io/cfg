package h8io.cfg

/** Source location of a config value (file name, line number, etc.).
  *
  * The format of [[description]] is determined by the backend that produced the node.
  */
trait Location {
  def description: String
}
