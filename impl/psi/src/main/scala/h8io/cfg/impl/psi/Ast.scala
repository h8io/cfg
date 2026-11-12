package h8io.cfg.impl.psi

import h8io.cfg.Id

/** The syntax tree of one source (`format.md` §4): fields in source order with their operators, nothing merged yet. */
private[psi] object Ast {

  /** A whole source: the fields of its top-level block. */
  final case class Document(statements: Vector[Statement])

  /** What a block holds: a field or a directive. */
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

  /** A directive in statement position, which is to produce a map merged into the block where it stands (§6).
    *
    * @param doc
    *   the doc comment before it, as written between its delimiters
    */
  final case class DirectiveStatement(call: Call, doc: Option[String]) extends Statement

  /** `#name(arguments)`, or `#name` alone with none, at `location`, the `#`. */
  final case class Call(name: String, arguments: Vector[Argument], location: SourceLocation)

  /** An argument, `name: value` or positional with no name; every value is a string, as the handler gets it (§6).
    *
    * @param location
    *   where the argument starts, its name or its value
    * @param valueLocation
    *   where its value starts
    */
  final case class Argument(
      name: Option[String],
      value: String,
      location: SourceLocation,
      valueLocation: SourceLocation)

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
    def location: PsiLocation
  }

  /** A scalar, quoted or not, its escapes read and its layout applied. */
  final case class Scalar(value: String, tags: Set[String], location: PsiLocation) extends Value

  /** `null`, unquoted. */
  final case class Null(tags: Set[String], location: PsiLocation) extends Value

  /** `{ … }` in value position. */
  final case class Block(statements: Vector[Statement], tags: Set[String], location: PsiLocation) extends Value

  /** `[ … ]`. */
  final case class Sequence(elements: Vector[Element], tags: Set[String], location: PsiLocation) extends Value

  /** `\${path}` or `\${?path}` unquoted and alone: the node it refers to, grafted whole (§9). */
  final case class Substitution(path: Id.Branch, optional: Boolean, tags: Set[String], location: SourceLocation)
      extends Value

  /** A scalar with `\${…}` inside it, or quoted around one alone, such as `"\${a}"`: always a scalar (§9).
    *
    * @param parts
    *   text and substitutions in the order written, as the lexer read them
    */
  final case class Concatenation(parts: Vector[Token.Part], tags: Set[String], location: SourceLocation) extends Value

  /** A directive in value position, which is to produce a node; the tags are added to that node's (§6). */
  final case class DirectiveValue(call: Call, tags: Set[String]) extends Value {
    def location: SourceLocation = call.location
  }

  /** An element of a sequence, with the doc comment before it. */
  final case class Element(value: Value, doc: Option[String])
}
