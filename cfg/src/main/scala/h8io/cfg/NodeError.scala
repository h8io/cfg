package h8io.cfg

/** A [[CfgError]] tied to a specific [[Node]].
  *
  * When processing a node produces an error, the error should carry that node so callers can report exactly where in
  * the tree the problem occurred. [[Node.INone]] and [[Node.INull]] implement this trait directly, since the nodes
  * themselves represent erroneous states.
  */
trait NodeError extends CfgError {
  def node: Node
}
