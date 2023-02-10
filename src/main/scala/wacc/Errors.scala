package wacc

object Errors {
  import wacc.AST._

  type ErrorInfoLines = Seq[String]
  type Position = (Int, Int)
  
  sealed trait Error
  case class SyntaxError(pos: Position, lines: ErrorInfoLines) extends Error {
    override def toString(): String = {
      "SyntaxError"
    }
  }
  // lines: [unxectedType, expectedType]
  case class TypeError(pos: Position, unexpectedType: String, expectedTypes: List[String]) extends Error {
    override def toString(): String = {
      "unexpected type: " + unexpectedType + "\n" +
      "expected type: " + expectedTypes + "\n"
    }
  }

  case class TypeMatchError(pos: Position, identifier: String, unexpectedType: String, expectedType: String) extends Error {
    override def toString(): String = {
      "identifier: " + identifier + "\n" +
      "unexpected type: " + unexpectedType + "\n" +
      "expected type: " + expectedType + "\n"
    }
  }

  case class BinaryOpAppTypeError(pos: Position, expectedTypes: List[String]) extends Error {
    override def toString(): String = {
      f"(${pos._1},${pos._2}) binary operator arguments should be of type: " + expectedTypes.mkString(", ") + "\n"
    }
  }

  case class ArrayIndicesError(pos: Position, expression: Expression) extends Error

  case class RedeclaredFunctionError(pos: Position, identifier: String) extends Error

  case class UndeclaredFunctionError(pos: Position, identifier: String) extends Error

  case class NewPairError(pos: Position, message: String) extends Error

  case class ArrayArityError(pos: Position, desiredArity: Int, expectedArity: Int) extends Error

  case class ArrayLiteralError(pos: Position, message: String) extends Error

  case class RValueError(pos: Position, message: String) extends Error

  case class UndeclaredVariableError(pos: Position, variableName: String) extends Error {
    override def toString(): String = {
      "variable " + variableName + " has not been declared in this scope" + "\n"
    }
  }

  case class FunctionCallError(pos: Position, args: Int, required: Int) extends Error

  case class RedeclaredVariableError(pos: Position, variableName: String) extends Error

  case class ReturnFromMainError(pos: Position) extends Error

  case class UnknownError(error: String) extends Error
}
