package h8io.cfg.impl.psi

/** The syntax tree of one source (`format.md` §4): fields in source order with their operators, nothing merged yet. */
private[psi] object Ast {

  /** A whole source: the fields of its top-level block. */
  final case class Document(statements: Vector[Statement])

  /** What a block holds: a field; directives come with step 4 of the plan. */
  sealed trait Statement

  /** `a.b.c: value`, `a.b.c := value` or `a.b.c { … }`, the last a `:` with a block for its value.
    *
    * @param keys
    *   the keys of a dotted key, outermost first; the operator applies to the last one (`format.md` §7)
    * @param doc
    *   the doc comment before the field, as written between its delimiters
    */
  final case class Field(keys: ::[Key], operator: Operator, value: Value, doc: Option[String]) extends Statement

  final case class Key(name: String, location: SourceLocation)

  sealed trait Operator

  object Operator {

    /** `:`, and the block form `key { … }`: maps merge. */
    case object Merge extends Operator

    /** `:=`: the value replaces whatever the key held. */
    case object Replace extends Operator
  }

  /** A value with the tags written before it; its location is where the value itself starts, past its tags. */
  sealed trait Value {
    def tags: Set[String]
    def location: SourceLocation
  }

  /** A scalar, quoted or not, its escapes read and its layout applied. */
  final case class Scalar(value: String, tags: Set[String], location: SourceLocation) extends Value

  /** `null`, unquoted. */
  final case class Null(tags: Set[String], location: SourceLocation) extends Value

  /** `{ … }` in value position. */
  final case class Block(statements: Vector[Statement], tags: Set[String], location: SourceLocation) extends Value

  /** `[ … ]`. */
  final case class Sequence(elements: Vector[Element], tags: Set[String], location: SourceLocation) extends Value

  /** An element of a sequence, with the doc comment before it. */
  final case class Element(value: Value, doc: Option[String])
}
