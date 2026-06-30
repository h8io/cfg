package h8io.cfg

/** A node in the config tree, tagged with its path type `I`.
  *
  * Every node in the tree is an `INode`. Use the type alias [[Node]] when the path type does not matter. Concrete
  * variants (all in the [[Node]] companion):
  *   - [[Node.INone]] — key was looked up but not found; also a [[NodeError]]
  *   - [[Node.INull]] — explicit null value in the source; also a [[NodeError]]
  *   - [[Node.IScalar]] — a leaf string value
  *   - [[Node.IMap]] / [[Node.ISeq]] — container nodes (map and sequence)
  *
  * @tparam I
  *   the [[Id]] subtype that describes this node's position in the tree
  */
sealed trait INode[+I <: Id] {
  def id: I
}

object Node {

  /** A missing key — the node was looked up but did not exist in the parent container.
    *
    * Also a [[NodeError]]; decoding fails when this node is encountered.
    *
    * @param id
    *   path at which the key was expected
    * @param parent
    *   the container that was queried
    */
  final case class INone[+I <: Id](id: I, parent: Container[?]) extends INode[I] with NodeError {
    def node: INone[I] = this
  }

  /** Type alias for [[INone]] with an unspecified path type. */
  type None = INone[Id]

  object None {
    def apply[I <: Id](id: I, parent: Container[?]): INone[I] = INone(id, parent)
    def unapply[I <: Id](node: INone[I]): Option[(I, IContainer[Id, ?])] = Some((node.id, node.parent))
  }

  /** A node that is physically present in the source (has a [[tag]] and a [[location]]). */
  sealed trait ISome[+I <: Id] extends INode[I] {

    /** Optional YAML tag (e.g. `!!str`). `None` when the backend does not produce tags. */
    def tag: Option[String]

    /** Source location of this node. */
    def location: Location
  }

  /** Type alias for [[ISome]] with an unspecified path type. */
  type Some = ISome[Id]

  /** An explicit `null` value in the source.
    *
    * Also a [[NodeError]]; decoders that do not explicitly handle null will fail.
    *
    * @param id
    *   path of this node
    * @param tag
    *   optional YAML tag
    * @param location
    *   source location
    */
  final case class INull[+I <: Id](id: I, tag: Option[String], location: Location) extends ISome[I] with NodeError {
    def node: INull[I] = this
  }

  /** Type alias for [[INull]] with an unspecified path type. */
  type Null = INull[Id]

  object Null {
    def apply[I <: Id](id: I, tag: Option[String], location: Location): INull[I] = INull(id, tag, location)
    def unapply[I <: Id](node: INull[I]): Option[(I, Option[String], Location)] =
      Some((node.id, node.tag, node.location))
  }

  /** A present, non-error node — either a scalar or a container. */
  sealed trait IValue[+I <: Id] extends ISome[I]

  /** Type alias for [[IValue]] with an unspecified path type. */
  type Value = IValue[Id]

  /** A leaf node holding a single string value.
    *
    * Scalars are always represented as strings; type conversion is the decoder's responsibility.
    *
    * @param id
    *   path of this node
    * @param tag
    *   optional YAML tag
    * @param value
    *   raw string representation
    * @param location
    *   source location
    */
  final case class IScalar[+I <: Id](id: I, tag: Option[String], value: String, location: Location) extends IValue[I]

  /** Type alias for [[IScalar]] with an unspecified path type. */
  type Scalar = IScalar[Id]

  object Scalar {
    def apply[I <: Id](id: I, tag: Option[String], value: String, location: Location): IScalar[I] =
      IScalar(id, tag, value, location)
    def unapply[I <: Id](node: IScalar[I]): Option[(I, Option[String], String, Location)] =
      Some((node.id, node.tag, node.value, node.location))
  }

  // TODO Drop it when we drop Scala 2.12 support
  private[Node] trait Iterable[+A] extends scala.Iterable[A] {
    def knownSize: Int
  }

  /** A container node (map or sequence) that is both a function from child ids to child nodes and an iterable over its
    * present children.
    *
    * @tparam I
    *   path type of this node
    * @tparam CI
    *   path type of child nodes
    */
  sealed trait IContainer[+I <: Id, CI <: Id] extends IValue[I] with (CI => INode[CI]) with Iterable[ISome[CI]] {
    override final def isEmpty: Boolean = size == 0
    override final def knownSize: Int = size
  }

  private type Container[CI <: Id] = IContainer[Id, CI]

  /** A map (object) node whose children are addressed by [[Id.Key]].
    *
    * Supports string-keyed lookup via the convenience overload `apply(key: String)` which constructs an [[Id.Key]]
    * relative to this node's own id. The `-` operator returns a copy of the map with the given key removed.
    */
  trait IMap[+I <: Id] extends IContainer[I, Id.Key] {
    def apply(key: Id.Key): INode[Id.Key]
    final def apply(key: String): INode[Id.Key] = apply(Id.Key(key, id))

    /** Returns this map without the entry for `key`. A no-op if `key` is absent. */
    def -(key: String): IMap[I]
  }

  /** Type alias for [[IMap]] with an unspecified path type. */
  type Map = IMap[Id]

  /** A sequence node whose children are addressed by [[Id.Index]].
    *
    * Supports integer-indexed lookup via the convenience overload `apply(index: Int)` which constructs an [[Id.Index]]
    * relative to this node's own id.
    */
  trait ISeq[+I <: Id] extends IContainer[I, Id.Index] {
    def apply(index: Id.Index): INode[Id.Index]
    final def apply(index: Int): INode[Id.Index] = apply(Id.Index(index, id))
  }

  /** Type alias for [[ISeq]] with an unspecified path type. */
  type Seq = ISeq[Id]
}
