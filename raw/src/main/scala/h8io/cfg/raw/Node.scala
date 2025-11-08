package h8io.cfg.raw

sealed trait Node[+I <: Id] {
  def id: I
  def origin: Origin
}

object Node {
  final case class None[+I <: Id](id: I, origin: Origin) extends Node[I]

  sealed trait Some[+I <: Id] extends Node[I]

  final case class Null[+I <: Id](id: I, origin: Origin) extends Some[I]

  final case class Scalar[+I <: Id](id: I, value: String, origin: Origin) extends Some[I]

  sealed trait Container[+I <: Id, CI <: Id] extends Some[I] with (CI => Node[CI]) with IterableOnce[Some[CI]] {
    def isEmpty: Boolean
    def size: Int
    def knownSize: Int
  }

  trait Map[I <: Id] extends Container[I, Id.Key] {
    def apply(key: Id.Key): Node[Id.Key]
    final def apply(key: String): Node[Id.Key] = apply(Id.Key(key, id))
  }

  trait Seq[I <: Id] extends Container[I, Id.Index] {
    def apply(key: Id.Index): Node[Id.Index]
    final def apply(i: Int): Node[Id.Index] = apply(Id.Index(i, id))
  }
}
