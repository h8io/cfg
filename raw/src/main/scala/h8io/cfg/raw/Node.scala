package h8io.cfg.raw

sealed trait Node {
  def id: Id
  def origin: Origin
}

object Node {
  final case class None(id: Id, origin: Origin) extends Node

  sealed trait Some extends Node

  final case class Null(id: Id, origin: Origin) extends Some

  final case class Scalar(id: Id, value: String, origin: Origin) extends Some

  sealed trait Container[CI <: Id] extends Some with (CI => Node) with IterableOnce[Some] {
    def isEmpty: Boolean
    def size: Int
    def knownSize: Int
  }

  trait Map extends Container[Id.Key] {
    def apply(key: Id.Key): Node
    final def apply(key: String): Node = apply(Id.Key(key, id))
  }

  trait Seq extends Container[Id.Index] {
    def apply(index: Id.Index): Node
    final def apply(index: Int): Node = apply(Id.Index(index, id))
  }
}
