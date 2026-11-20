package h8io.cfg.impl.psi

import h8io.cfg.Id

/** The syntax tree of one source (`format.md` §4): fields in source order with their operators, nothing merged yet. */
private[psi] object AST {

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

  /** An injection, `$path` or `${path}` alone among the fields of a block: the fields of the map at `path`, merged
    * there as if written there (§7, §9).
    */
  final case class InjectionStatement(path: Id.Branch, optional: Boolean, location: SourceLocation, doc: Option[String])
      extends Statement

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
    * element of a sequence (§9).
    */
  final case class Substitution(path: Id.Branch, optional: Boolean, tag: Option[String], location: SourceLocation)
      extends Value

  /** A block opened by indentation that holds substitutions and calls alone, two or more, none tagged: what the first
    * that finds or gives anything finds or gives decides what it is, the fields of maps merged in turn or the elements
    * of sequences (§4, §9).
    *
    * @param items
    *   in the order written
    */
  final case class Injections(items: Vector[Injected], tag: Option[String], location: SourceLocation) extends Value

  /** What a block of injections alone holds, in the order written (§9). */
  sealed trait Injected

  object Injected {

    /** A call, not expanded yet. */
    final case class Calling(call: Call) extends Injected

    /** What a call gave once expanded, as it would stand as a value: kept until substitutions are resolved, so that
      * what decides the block is the first that finds or gives anything as written, a call or a substitution.
      */
    final case class Given(call: Call, value: Value) extends Injected

    /** A substitution. */
    final case class Referring(reference: Token.Reference) extends Injected
  }

  /** A scalar with `\${…}` inside it, or quoted around one alone, such as `"\${a}"`: always a scalar (§9).
    *
    * @param parts
    *   text and substitutions in the order written, as the lexer read them
    */
  final case class Concatenation(parts: Vector[Token.Part], tag: Option[String], location: SourceLocation) extends Value

  /** A block, a scalar or a concatenation, with the loader's markers to run over its value once its substitutions are
    * resolved (§5).
    */
  final case class Marked(value: Value, markers: Vector[MarkerParser.MarkerCall]) extends Value {
    def tag: Option[String] = value.tag
    def location: PsiLocation = value.location
  }

  /** A directive in value position, which is to produce a node; the tag replaces that node's (§6). */
  final case class DirectiveValue(call: Call, tag: Option[String]) extends Value {
    def location: SourceLocation = call.location
  }

  /** How an element that is a substitution or a call stands among the elements of a sequence (§6, §9). */
  sealed trait Put

  object Put {

    /** As one element: after a `- `. */
    case object Whole extends Put

    /** In `[…]`, an injection: the elements of a sequence found or given, anything else as one element. */
    case object Found extends Put

    /** Alone on its line in a block of `- ` elements, an injection: the elements of a sequence, which it must be. */
    case object Elements extends Put
  }

  /** An element of a sequence, with the doc comment before it; `put` says how it stands there when it is a substitution
    * or a call, and means nothing for any other value.
    */
  final case class Element(value: Value, doc: Option[String], put: Put = Put.Whole)
}
