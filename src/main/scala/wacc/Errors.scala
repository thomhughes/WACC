package wacc

object Errors {
  type ErrorInfoLines = Seq[String]
  case class Position(line: Int, col: Int)

  case class SyntaxError(pos: Position, lines: ErrorInfoLines) {
    override def toString(): String = {
      "SyntaxError"
    }
  }
}
