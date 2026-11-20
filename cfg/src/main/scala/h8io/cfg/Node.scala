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
    * Also a [[NodeError]].
    *
    * @param id
    *   path at which the key was expected
    */
  final case class INone[+I <: Id](id: I) extends INode[I] with NodeError {
    def node: INone[I] = this
  }

  /** Type alias for [[INone]] with an unspecified path type. */
  type None = INone[Id]

  object None {
    def apply[I <: Id](id: I): INone[I] = INone(id)
    def unapply[I <: Id](node: INone[I]): Option[I] = Some(node.id)
  }

  /** A node that is physically present in the source (has a [[tag]] and a [[location]]). */
  sealed trait ISome[+I <: Id] extends INode[I] {

    /** The tag written on this node in the source, by name, without the syntax that marks it: `@duration` in Psi and
      * `!duration` in YAML both give `duration`. Nothing is ever inferred, so it is `None` when no tag was written or
      * the backend has none.
      */
    def tag: Option[String]

    /** Source location of this node. */
    def location: Location
  }

  /** Type alias for [[ISome]] with an unspecified path type. */
  type Some = ISome[Id]

  /** An explicit `null` value in the source.
    *
    * Also a [[NodeError]].
    *
    * @param id
    *   path of this node
    * @param tag
    *   the tag written on the node
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

  /** A leaf node holding a single string value.
    *
    * Scalars are always represented as strings; converting them is up to the consumer.
    *
    * @param id
    *   path of this node
    * @param tag
    *   the tag written on the node
    * @param value
    *   raw string representation
    * @param location
    *   source location
    */
  final case class IScalar[+I <: Id](id: I, tag: Option[String], value: String, location: Location) extends ISome[I]

  /** Type alias for [[IScalar]] with an unspecified path type. */
  type Scalar = IScalar[Id]

  object Scalar {
    def apply[I <: Id](id: I, tag: Option[String], value: String, location: Location): IScalar[I] =
      IScalar(id, tag, value, location)
    def unapply[I <: Id](node: IScalar[I]): Option[(I, Option[String], String, Location)] =
      Some((node.id, node.tag, node.value, node.location))
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
  sealed trait IContainer[+I <: Id, +CI <: Id] extends ISome[I] with Iterable[ISome[CI]] {
    override final def isEmpty: Boolean = size == 0
    override final def knownSize: Int = size
  }

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
