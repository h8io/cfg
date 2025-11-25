package h8io.cfg.raw

import h8io.cfg.CfgError

sealed trait Node[+I <: Id] {
  def id: I
}

object Node {
  final case class None[+I <: Id](id: I, parent: Node.Container[Id, ?]) extends Node[I] with CfgError {
    def node: Node.None[I] = this
  }

  sealed trait Some[+I <: Id] extends Node[I] {
    def location: Location
  }

  final case class Null[+I <: Id](id: I, location: Location) extends Some[I] with CfgError {
    def node: Node.Null[I] = this
  }

  sealed trait Value[+I <: Id] extends Some[I]

  final case class Scalar[+I <: Id](id: I, value: String, location: Location) extends Value[I]

  sealed trait Container[+I <: Id, CI <: Id] extends Value[I] with (CI => Node[CI]) {
    def iterator: Iterator[Some[CI]]
    def size: Int
    final def isEmpty: Boolean = size == 0
  }

  trait Map[+I <: Id] extends Container[I, Id.Key] {
    def apply(key: Id.Key): Node[Id.Key]
    final def apply(key: String): Node[Id.Key] = apply(Id.Key(key, id))
  }

  trait Seq[+I <: Id] extends Container[I, Id.Index] {
    def apply(index: Id.Index): Node[Id.Index]
    final def apply(index: Int): Node[Id.Index] = apply(Id.Index(index, id))
  }
}
