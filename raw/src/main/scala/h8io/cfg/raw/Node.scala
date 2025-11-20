package h8io.cfg.raw

import h8io.cfg.CfgError

sealed trait Node {
  def id: Id
}

object Node {
  final case class None(id: Id, parent: Node.Container[?]) extends Node with CfgError {
    def node: Node.None = this
  }

  sealed trait Some extends Node {
    def location: Location
  }

  final case class Null(id: Id, location: Location) extends Some

  final case class Scalar(id: Id, value: String, location: Location) extends Some

  sealed trait Container[CI <: Id] extends Some with (CI => Node) {
    def iterator: Iterator[Some]
    def size: Int
    final def isEmpty: Boolean = size == 0
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
