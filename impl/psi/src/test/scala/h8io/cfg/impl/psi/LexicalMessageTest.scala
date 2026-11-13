package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException

class LexicalMessageTest extends AnyFlatSpec with Matchers {
  private val at = SourceLocation("s", 1, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "An unreadable source" should "say why" in {
    text(SourceUnreadable("s", new IOException("gone"))) shouldBe
      (("the source cannot be read: java.io.IOException: gone", None))
  }

  "Characters" should "be named, with what to do about them" in {
    text(InvalidEncoding(at)) shouldBe
      (("a byte that is not UTF-8; the rest of this source is not read", Some("save the source as UTF-8")))
    text(LoneCarriageReturn(at)) shouldBe
      (("a carriage return, U+000D CARRIAGE RETURN (CR), with no line feed after it",
        Some("end lines with LF or CRLF")))
    text(ForeignLineBreak(at, ' ')) shouldBe
      ((
        "a line break other than LF and CRLF, U+2028 LINE SEPARATOR",
        Some("end lines with LF or CRLF; in a string, write it as \\u2028")))
    text(ControlCharacter(at, '\u007F')) shouldBe
      (("a control character, U+007F DELETE", Some("remove it, or in \"…\" write it as \\u007F")))
    text(InvisibleCharacter(at, '​')) shouldBe
      (("an invisible character, U+200B ZERO WIDTH SPACE", Some("remove it, or in \"…\" write it as \\u200B")))
    text(ForeignWhitespace(at, ' ')) shouldBe
      (("whitespace other than space and tab, U+00A0 NO-BREAK SPACE", Some("write a space instead")))
    text(TabInIndentation(at)) shouldBe (("a tab in the indentation", Some("indent with spaces")))
  }

  "Comments" should "say what is unclosed or stray" in {
    text(UnterminatedComment(at)) shouldBe
      ((
        "a comment that is never closed; the rest of this source is not read",
        Some("close it with '*/'; comments nest, so each '/*' inside needs its own")))
    text(StrayCommentEnd(at)) shouldBe (("'*/' outside any comment", None))
  }

  "An unterminated string" should "name its delimiter, and say a one-line string closes on its line" in {
    text(UnterminatedString(at, "\"")) shouldBe
      ((
        "a string opened with \" is never closed on its line; the rest of this source is not read",
        Some("close it on its line; a value on several lines is written in \"\"\"…\"\"\"")))
    text(UnterminatedString(at, "'")) shouldBe
      ((
        "a string opened with ' is never closed on its line; the rest of this source is not read",
        Some("close it on its line; a value on several lines is written in '''…'''")))
    text(UnterminatedString(at, "\"\"\"")) shouldBe
      (("a string opened with \"\"\" is never closed; the rest of this source is not read",
        Some("close it with \"\"\"")))
    text(UnterminatedName(at)) shouldBe
      ((
        "a name in backticks that is never closed on its line; the rest of this source is not read",
        Some("close it with a backtick on its line")))
  }

  "Escapes and keys" should "show what was written" in {
    text(InvalidEscape(at, "\\q")) shouldBe (("unknown escape '\\q'", Some("write a backslash as '\\\\'")))
    text(InvalidKey(at, "1a")) shouldBe (("'1a' cannot be a bare key", Some("quote it in backticks: `1a`")))
    text(ColonWithoutSpace(at)) shouldBe
      (("no space after the colon", Some("put a space after it, as in 'key: value'")))
    text(QuoteInUnquotedScalar(at)) shouldBe
      (("a quote inside an unquoted value", Some("quote the whole value in \"…\"")))
  }

  "Tags and directives" should "say what is missing or reserved" in {
    text(MissingTagName(at)) shouldBe (("'@' with no tag name after it", None))
    text(ReservedTagRemoval(at)) shouldBe (("'@-' is reserved for removing an inherited tag", None))
    text(TagArguments(at)) shouldBe (("arguments to a tag, which takes none", None))
    text(MissingDirectiveName(at)) shouldBe
      (("'#' with no directive name after it", Some("a comment is written '//' or '/* … */'")))
  }

  "Substitutions" should "say what cannot be read" in {
    text(WhitespaceInSubstitution(at)) shouldBe (("whitespace inside '${" + "…}'", Some("remove it")))
    text(InvalidSubstitution(at)) shouldBe
      ((
        "the path of a substitution cannot be read",
        Some("a path is keys joined by dots and indexes in brackets, a key that is not bare in backticks")))
  }
}
