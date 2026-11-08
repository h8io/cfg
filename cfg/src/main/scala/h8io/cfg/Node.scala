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
    * Also a [[NodeError]]; decoding fails when this node is encountered. Only a container creates one, from its own id,
    * so `id.parent` is always `parent.id`. That is why this is not a case class: Scala 2 would keep its `apply` and
    * `copy` public whatever the constructor's access.
    *
    * @param id
    *   path at which the key was expected
    * @param parent
    *   the container that was queried
    */
  final class INone[+I <: Id] private[cfg] (val id: I, val parent: Container[?]) extends INode[I] with NodeError {
    def node: INone[I] = this

    override def equals(that: Any): Boolean =
      that match {
        case none: INone[?] => id == none.id && parent == none.parent
        case _ => false
      }

    override def hashCode: Int = (id, parent).##

    override def toString: String = s"INone(${id.toString}, ${parent.toString})"
  }

  object INone {
    private[cfg] def apply[I <: Id](id: I, parent: Container[?]): INone[I] = new INone(id, parent)
    def unapply[I <: Id](node: INone[I]): Option[(I, IContainer[Id, ?])] = Some((node.id, node.parent))
  }

  /** Type alias for [[INone]] with an unspecified path type. */
  type None = INone[Id]

  object None {
    private[cfg] def apply[I <: Id](id: I, parent: Container[?]): INone[I] = INone(id, parent)
    def unapply[I <: Id](node: INone[I]): Option[(I, IContainer[Id, ?])] = INone.unapply(node)
  }

  /** A node that is physically present in the source (has [[tags]] and a [[location]]). */
  sealed trait ISome[+I <: Id] extends INode[I] {

    /** The tags written on this node in the source, by name, without the syntax that marks them: `@duration` in Psi and
      * `!duration` in YAML both give `duration`. Order carries no meaning, and nothing is ever inferred, so the set is
      * empty when no tag was written or the backend has none.
      */
    def tags: Set[String]

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
    * @param tags
    *   the tags written on the node
    * @param location
    *   source location
    */
  final case class INull[+I <: Id](id: I, tags: Set[String], location: Location) extends ISome[I] with NodeError {
    def node: INull[I] = this
  }

  /** Type alias for [[INull]] with an unspecified path type. */
  type Null = INull[Id]

  object Null {
    def apply[I <: Id](id: I, tags: Set[String], location: Location): INull[I] = INull(id, tags, location)
    def unapply[I <: Id](node: INull[I]): Option[(I, Set[String], Location)] =
      Some((node.id, node.tags, node.location))
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
    * @param tags
    *   the tags written on the node
    * @param value
    *   raw string representation
    * @param location
    *   source location
    */
  final case class IScalar[+I <: Id](id: I, tags: Set[String], value: String, location: Location) extends IValue[I]

  /** Type alias for [[IScalar]] with an unspecified path type. */
  type Scalar = IScalar[Id]

  object Scalar {
    def apply[I <: Id](id: I, tags: Set[String], value: String, location: Location): IScalar[I] =
      IScalar(id, tags, value, location)
    def unapply[I <: Id](node: IScalar[I]): Option[(I, Set[String], String, Location)] =
      Some((node.id, node.tags, node.value, node.location))
  }

  // TODO Drop it when we drop Scala 2.12 support
  private[Node] trait Iterable[+A] extends scala.Iterable[A] {
    def knownSize: Int
  }

  /** A container node (map or sequence): an iterable over its present children.
    *
    * A child is looked up by its segment alone — a key or an index — and the container builds the child's id from its
    * own, so a node it returns, missing or not, always sits directly below it.
    *
    * @tparam I
    *   path type of this node
    * @tparam CI
    *   path type of child nodes
    */
  sealed trait IContainer[+I <: Id, CI <: Id] extends IValue[I] with Iterable[ISome[CI]] {
    override final def isEmpty: Boolean = size == 0
    override final def knownSize: Int = size
  }

  private type Container[CI <: Id] = IContainer[Id, CI]

  /** A map (object) node whose children are addressed by [[Id.Key]].
    *
    * The `-` operator returns a copy of the map with the given key removed.
    */
  trait IMap[+I <: Id] extends IContainer[I, Id.Key] {

    /** The child at `key`, with the id `Id.Key(key, id)`; an [[INone]] if there is none. */
    def apply(key: String): INode[Id.Key]

    /** Returns this map without the entry for `key`. A no-op if `key` is absent. */
    def -(key: String): IMap[I]
  }

  /** Type alias for [[IMap]] with an unspecified path type. */
  type Map = IMap[Id]

  /** A sequence node whose children are addressed by [[Id.Index]]. */
  trait ISeq[+I <: Id] extends IContainer[I, Id.Index] {

    /** The child at `index`, with the id `Id.Index(index, id)`; an [[INone]] if `index` is out of bounds. */
    def apply(index: Int): INode[Id.Index]
  }

  /** Type alias for [[ISeq]] with an unspecified path type. */
  type Seq = ISeq[Id]
}
