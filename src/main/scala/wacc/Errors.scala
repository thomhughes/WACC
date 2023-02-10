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
  // lines: [unxectedType, expectedType]
  case class TypeError(pos: Position, lines: ErrorInfoLines) extends Error {
    override def toString(): String = {
      "Type error in FILENAMEHERE at position " + pos.toString() + "\n" +
      "unexpected type: " + lines(0) + "\n" +
      "expected type: " + lines(1) + "\n"
    }
  }

  case class UndeclaredVariableError (pos: Position, lines: ErrorInfoLines) extends Error {
    override def toString(): String = {
      "Undeclared variable error in FILENAMEHERE at position " + pos.toString() + "\n" +
      "variable " + lines(0) + " has not been declared in this scope" + "\n"
    }
  }
}
