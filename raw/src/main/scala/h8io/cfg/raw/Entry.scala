package h8io.cfg.raw

sealed trait Entry[+P <: Ref] {
  def id: P
  def origin: Origin
}

object Entry {
  final case class None[+P <: Ref](id: P, origin: Origin) extends Entry[P]

  final case class Null[+P <: Ref](id: P, origin: Origin) extends Entry[P]

  final case class Scalar[+P <: Ref](id: P, value: String, origin: Origin) extends Entry[P]

  sealed trait Container[+P <: Ref, CP <: Ref] extends Entry[P] with (CP => Entry[CP]) with Iterable[Entry[CP]] {
    def isEmpty: Boolean
    def size: Int
    def knownSize: Int
  }

  trait Map[P <: Ref] extends Container[P, Ref.Key] {
    final def apply(key: String): Entry[Ref.Key] = apply(Ref.Key(key))
  }

  trait Seq[P <: Ref] extends Container[P, Ref.Index] {
    final def apply(i: Int): Entry[Ref.Index] = apply(Ref.Index(i))
  }
}
