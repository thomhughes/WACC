package wacc

object Errors {
  type ErrorInfoLines = Seq[String]
  case class Position(line: Int, col: Int)
  
  sealed trait Error
  case class SyntaxError(pos: Position, lines: ErrorInfoLines) extends Error {
    override def toString(): String = {
      "SyntaxError"
    }
  }
}
