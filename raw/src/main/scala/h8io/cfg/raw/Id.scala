package h8io.cfg.raw

sealed trait Id {
  def description: String
}

object Id {
  case object Root extends Id {
    def description: String = "root"
  }

  final case class Key(key: String) extends Id {
    def description: String = s"key $key"
  }

  final case class Index(index: Int) extends Id {
    @inline def fits(n: Int): Boolean = index >= 0 && index < n

    def description: String = s"index $index"
  }
}
