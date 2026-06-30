package h8io

package object cfg {

  /** Type alias for [[INode]] with an unspecified path type, used throughout the API when the concrete [[Id]] subtype
    * does not matter.
    */
  type Node = INode[Id]
}
