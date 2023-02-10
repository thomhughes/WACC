package wacc

object Errors {
  import wacc.AST._

  type ErrorInfoLines = Seq[String]
  type Position = (Int, Int)
  
  sealed trait Error {
    def generateError(fileName: String): String
  }
  case class SyntaxError(pos: Position, lines: ErrorInfoLines) extends Error {
    override def generateError(fileName: String) =
      lines.foldLeft("")((acc, line) => acc + line + "\n")
  }
  
  case class TypeError(pos: Position, unexpectedType: String, expectedTypes: List[String]) extends Error {
    override def generateError(fileName: String) = {
      "Type error in " + fileName + " " + pos.toString + "\n" +
      pos.toString + "unexpected type: " + unexpectedType + "\n" +
      "expected type: " + expectedTypes + "\n"
    }
  }

  case class TypeMatchError(pos: Position, identifier: String, unexpectedType: String, expectedType: String) extends Error {
    override def generateError(fileName: String) = {
      "Type error in " + fileName + " for identifier " + identifier + " " + pos.toString + ":\n"
      "unexpected type: " + unexpectedType + "\n" +
      "expected type: " + expectedType + "\n"
    }
  }

  case class BinaryOpAppTypeError(pos: Position, expectedTypes: List[String]) extends Error {
    override def generateError(fileName: String) = {
      f"(${pos._1},${pos._2}) binary operator arguments should be of type: " + expectedTypes.mkString(", ") + "\n"
    }
  }

  case class ReadStatementError(pos: Position) extends Error {
    override def generateError(fileName: String): String = ???
  }

  case class ArrayIndicesError(pos: Position, expression: Expression) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class RedeclaredFunctionError(pos: Position, identifier: String) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class UndeclaredFunctionError(pos: Position, identifier: String) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class NewPairError(pos: Position, message: String) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class ArrayArityError(pos: Position, desiredArity: Int, expectedArity: Int) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class ArrayLiteralError(pos: Position, message: String) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class RValueError(pos: Position, message: String) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class UndeclaredVariableError(pos: Position, variableName: String) extends Error {
    override def generateError(fileName: String): String = {
      "Scope error in " + fileName + " " + pos.toString + ":\n" +
      "variable " + variableName + " has not been declared in this scope" + "\n"
    }
  }

  case class FunctionCallError(pos: Position, args: Int, required: Int) extends Error {
    override def generateError(fileName: String): String = ???

  }

  case class RedeclaredVariableError(pos: Position, variableName: String) extends Error {
    override def generateError(fileName: String): String = ???
  }

  case class ReturnFromMainError(pos: Position) extends Error {
    override def generateError(fileName: String): String = ???
  }

  case class UnknownError(error: String) extends Error {
    override def generateError(fileName: String): String = ???
  }
}
