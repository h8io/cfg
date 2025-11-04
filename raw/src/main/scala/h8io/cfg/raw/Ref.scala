package h8io.cfg.raw

sealed trait Ref {
  def description: String
}

object Ref {
  case object Root extends Ref {
    def description: String = "root"
  }

  final case class Key(key: String) extends Ref {
    def description: String = s"key $key"
  }

  final case class Index(index: Int) extends Ref {
    @inline def fits(n: Int): Boolean = index >= 0 && index < n

    def description: String = s"index $index"
  }
}
