package h8io.cfg.raw

import h8io.cfg.CfgError

sealed trait INode[+I <: Id] {
  def id: I
}

object Node {
  final case class INone[+I <: Id](id: I, parent: Container[?]) extends INode[I] with CfgError {
    def node: INone[I] = this
  }

  type None = INone[Id]

  object None {
    def apply[I <: Id](id: I, parent: Container[?]): INone[I] = INone(id, parent)
    def unapply(node: None): Option[(Id, IContainer[Id, ?])] = Some((node.id, node.parent))
  }

  sealed trait ISome[+I <: Id] extends INode[I] {
    def location: Location
  }

  type Some = ISome[Id]

  final case class INull[+I <: Id](id: I, location: Location) extends ISome[I] with CfgError {
    def node: INull[I] = this
  }

  type Null = INull[Id]

  object Null {
    def apply[I <: Id](id: I, location: Location): INull[I] = INull(id, location)
    def unapply(node: Null): Option[(Id, Location)] = Some((node.id, node.location))
  }

  sealed trait IValue[+I <: Id] extends ISome[I]

  type Value = IValue[Id]

  final case class IScalar[+I <: Id](id: I, value: String, location: Location) extends IValue[I]

  type Scalar = IScalar[Id]

  object Scalar {
    def apply[I <: Id](id: I, value: String, location: Location): IScalar[I] = IScalar(id, value, location)
    def unapply(node: Scalar): Option[(Id, String, Location)] = Some((node.id, node.value, node.location))
  }

  sealed trait IContainer[+I <: Id, CI <: Id] extends IValue[I] with (CI => INode[CI]) {
    def iterator: Iterator[ISome[CI]]
    def size: Int
    final def isEmpty: Boolean = size == 0
  }

  type Container[CI <: Id] = IContainer[Id, CI]

  trait IMap[+I <: Id] extends IContainer[I, Id.Key] {
    def apply(key: Id.Key): INode[Id.Key]
    final def apply(key: String): INode[Id.Key] = apply(Id.Key(key, id))
  }

  type Map = IMap[Id]

  trait ISeq[+I <: Id] extends IContainer[I, Id.Index] {
    def apply(index: Id.Index): INode[Id.Index]
    final def apply(index: Int): INode[Id.Index] = apply(Id.Index(index, id))
  }

  type Seq = ISeq[Id]
}
