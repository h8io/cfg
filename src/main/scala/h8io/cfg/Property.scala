package h8io.cfg

trait Property[T] extends (CfgMap => CfgResult[T]) {
  def name: String
}
