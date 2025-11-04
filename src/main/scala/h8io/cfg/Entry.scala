package h8io.cfg

sealed trait Entry[+P <: Path] {
  def path: P
  def origin: Origin
}

object Entry {
  final case class None[+P <: Path](path: P, origin: Origin) extends Entry[P]

  final case class Null[+P <: Path](path: P, origin: Origin) extends Entry[P]

  final case class Scalar[+P <: Path](path: P, value: String, origin: Origin) extends Entry[P]

  sealed trait Container[+P <: Path, CP <: Path] extends Entry[P] with (CP => Entry[CP]) with Iterable[Entry[CP]] {
    def isEmpty: Boolean
    def size: Int
    def knownSize: Int
  }

  trait Map[P <: Path] extends Container[P, Path.Key] {
    final def apply(key: String): Entry[Path.Key] = apply(Path.Key(key))
  }

  trait Seq[P <: Path] extends Container[P, Path.Index] {
    final def apply(i: Int): Entry[Path.Index] = apply(Path.Index(i))
  }
}
