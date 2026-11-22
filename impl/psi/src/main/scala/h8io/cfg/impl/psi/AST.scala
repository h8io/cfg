package h8io.cfg.impl.psi

import h8io.cfg.Id

/** The syntax tree of one source: fields in source order with their operators, nothing merged yet. */
private[psi] object AST {

  /** A whole source: the fields of its top-level block. */
  final case class Document(statements: Vector[Statement])

  /** What a block holds: a field or a directive. */
  sealed trait Statement

  /** `a.b.c: value`, `a.b.c := value` or `a.b.c { … }`, the last a `:` with a block for its value.
    *
    * @param keys
    *   the keys of a dotted key, outermost first; the operator applies to the last one
    * @param doc
    *   the doc comment before the field, as written between its delimiters
    */
  final case class Field(keys: ::[Key], operator: Operator, value: Value, doc: Option[String]) extends Statement

  final case class Key(name: String, location: SourceLocation)

  /** A directive in statement position, `#name(…)*`, which is to produce a map merged into the block where it stands.
    *
    * @param doc
    *   the doc comment before it, as written between its delimiters
    */
  final case class DirectiveStatement(call: Call, doc: Option[String]) extends Statement

  /** An injection, `$path*` or `${path*}` alone among the fields of a block: the fields of the map at `path`, merged
    * there as if written there.
    */
  final case class InjectionStatement(path: Id.Branch, optional: Boolean, location: SourceLocation, doc: Option[String])
      extends Statement

  /** `#name(arguments)`, or `#name` alone with none, at `location`, the `#`. */
  final case class Call(name: String, arguments: Vector[Argument], location: SourceLocation)

  /** An argument, `name: value` or positional with no name; every value is a string, as the handler gets it.
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

  /** A value with the tag written before it; its location is where the value itself starts, past its tag. */
  sealed trait Value {
    def tag: Option[String]
    def location: PsiLocation
  }

  /** A scalar, quoted or not, its escapes read and its layout applied. */
  final case class Scalar(value: String, tag: Option[String], location: PsiLocation) extends Value

  /** `null`, unquoted. */
  final case class Null(tag: Option[String], location: PsiLocation) extends Value

  /** `{ … }` in value position. */
  final case class Block(statements: Vector[Statement], tag: Option[String], location: PsiLocation) extends Value

  /** `[ … ]`. */
  final case class Sequence(elements: Vector[Element], tag: Option[String], location: PsiLocation) extends Value

  /** `\${path}` or `\${?path}` unquoted and alone: the node it refers to, grafted whole, as a field's value or as an
    * element of a sequence.
    */
  final case class Substitution(path: Id.Branch, optional: Boolean, tag: Option[String], location: SourceLocation)
      extends Value

  /** An injection after a key, `a: $b*` or `a: #d(…)*`: a node of what it finds or gives, a map holding its fields or a
    * sequence holding its elements, with the tag written before it and none of the found node's own.
    */
  final case class Contents(item: Injected, tag: Option[String], location: SourceLocation) extends Value

  /** What an injection after a key injects. */
  sealed trait Injected

  object Injected {

    /** A call, not expanded yet. */
    final case class Calling(call: Call) extends Injected

    /** What a call gave once expanded, as it would stand as a value: kept until substitutions are resolved, as what a
      * substitution finds is.
      */
    final case class Given(call: Call, value: Value) extends Injected

    /** A substitution. */
    final case class Referring(reference: Token.Reference) extends Injected
  }

  /** A scalar with `\${…}` inside it, or quoted around one alone, such as `"\${a}"`: always a scalar.
    *
    * @param parts
    *   text and substitutions in the order written, as the lexer read them
    */
  final case class Concatenation(parts: Vector[Token.Part], tag: Option[String], location: SourceLocation) extends Value

  /** `\${path; name(…)}` unquoted and alone: the text the formatter gives for the node found, a scalar; nothing when an
    * optional one finds nothing.
    */
  final case class Formatted(part: Token.Formatted, tag: Option[String], location: SourceLocation) extends Value

  /** A block, a scalar or a concatenation, with the loader's markers to run over its value once its substitutions are
    * resolved.
    */
  final case class Marked(value: Value, markers: Vector[MarkerParser.MarkerCall]) extends Value {
    def tag: Option[String] = value.tag
    def location: PsiLocation = value.location
  }

  /** A directive in value position, which is to produce a node; the tag replaces that node's. */
  final case class DirectiveValue(call: Call, tag: Option[String]) extends Value {
    def location: SourceLocation = call.location
  }

  /** How an element that is a substitution or a call stands among the elements of a sequence. */
  sealed trait Put

  object Put {

    /** As one element: after a `- ` or in `[…]`, unmarked. */
    case object Whole extends Put

    /** Marked with a `*`, after a `- ` or in `[…]`, an injection: the elements of a sequence, which it must be. */
    case object Elements extends Put
  }

  /** An element of a sequence, with the doc comment before it; `put` says how it stands there when it is a substitution
    * or a call, and means nothing for any other value.
    */
  final case class Element(value: Value, doc: Option[String], put: Put = Put.Whole)
}
