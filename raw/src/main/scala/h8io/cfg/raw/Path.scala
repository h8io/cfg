package h8io.cfg.raw

sealed trait Path {
  def description: String
}

object Path {
  case object Root extends Path {
    override def description: String = "root"
  }

  final case class Key(key: String) extends Path {
    def description: String = s"key $key"
  }

  final case class Index(index: Int) extends Path {
    @inline def fits(n: Int): Boolean = index >= 0 && index < n

    def description: String = s"index $index"
  }
}
