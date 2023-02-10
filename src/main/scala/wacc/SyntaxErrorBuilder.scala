package wacc

import wacc.Errors.SyntaxError
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.TillNextWhitespace

object SyntaxErrorBuilder
    extends ErrorBuilder[SyntaxError]
    with TillNextWhitespace {
  import scala.util.matching.Regex

  override def format(pos: Position,
                      source: Source,
                      lines: ErrorInfoLines): SyntaxError =
    SyntaxError(pos, lines)

  type Position = Errors.Position
  type Source = Unit

  override def pos(line: Int, col: Int): Position = (line, col)

  override def source(sourceName: Option[String]): Source = ()

  type ErrorInfoLines = Seq[String]

  override def vanillaError(unexpected: UnexpectedLine,
                            expected: ExpectedLine,
                            reasons: Messages,
                            lines: LineInfo): ErrorInfoLines = {
    val reasons_ = reasons.collect {
      case reason if reason.nonEmpty => Some(reason)
    }
    combineOrUnknown((unexpected +: expected +: reasons_).flatten, lines)
  }

  override def specialisedError(msgs: Messages,
                                lines: LineInfo): ErrorInfoLines =
    combineOrUnknown(msgs, lines)

  private val Unknown = "unknown parse error"
  private def combineOrUnknown(info: Seq[String],
                               lines: Seq[String]): ErrorInfoLines = {
    if (info.isEmpty) Unknown +: lines
    else info ++: lines
  }

  type ExpectedItems = Option[String]

  type Messages = Seq[Message]

  override def combineExpectedItems(alts: Set[Item]): ExpectedItems =
    alts.toList.filter(_.nonEmpty).sorted.reverse match {
      case Nil              => None
      case List(alt)        => Some(alt)
      case List(alt1, alt2) => Some(s"$alt2 or $alt1")
      // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
      case any @ (alt :: alts) if any.exists(_.contains(",")) =>
        Some(s"${alts.reverse.mkString("; ")}; or $alt")
      case alt :: alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
    }

  override def combineMessages(alts: Seq[Message]): Messages =
    alts.filter(_.nonEmpty)

  type UnexpectedLine = Option[String]

  type ExpectedLine = Option[String]

  type Message = String

  type LineInfo = Seq[String]
  override def unexpected(item: Option[Item]): UnexpectedLine =
    item.map("unexpected " + _)
  override def expected(alts: ExpectedItems): ExpectedLine =
    alts.map("expected " + _)
  override def reason(reason: String): Message = reason
  override def message(msg: String): Message = msg

  override val numLinesBefore = 1
  override val numLinesAfter = 1
  override val trimToParserDemand = false
  override def lineInfo(line: String,
                        linesBefore: Seq[String],
                        linesAfter: Seq[String],
                        errorPointsAt: Int,
                        errorWidth: Int): LineInfo = {
    linesBefore.map(line => s"$errorLineStart$line") ++:
      Seq(
      s"$errorLineStart$line",
      s"${" " * errorLineStart.length}${errorPointer(errorPointsAt, errorWidth)}") ++:
      linesAfter.map(line => s"$errorLineStart$line")
  }

  private val errorLineStart = "|"
  private def errorPointer(caretAt: Int, caretWidth: Int) =
    s"${" " * caretAt}${"^" * caretWidth}"

  type Item = String
  type Raw = String
  type Named = String
  type EndOfInput = String

  private val Unprintable: Regex = "(\\p{C})".r
  private object WhitespaceOrUnprintable {
    def unapply(cs: Iterable[Char]): Option[String] = unapply(cs.head)
    def unapply(s: String): Option[String] = unapply(s.charAt(0))
    def unapply(c: Char): Option[String] = c match {
      case '\n'                   => Some("newline")
      case '\t'                   => Some("tab")
      case c if c.isSpaceChar     => Some("space")
      case c if c.isWhitespace    => Some("whitespace character")
      case c if c.isHighSurrogate => None
      case Unprintable(up) =>
        Some(f"unprintable character (\\u${up.toInt}%04X)")
      case _ => None
    }
  }

  override def raw(item: String): Raw = item match {
    case WhitespaceOrUnprintable(name) => name
    // this will handle utf-16 surrogate pairs properly
    case cs => "\"" + cs + "\""
  }

  override def named(item: String): Named = item
  override val endOfInput: EndOfInput = "end of input"
}
