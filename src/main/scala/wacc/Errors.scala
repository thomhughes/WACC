package wacc

object Errors {
  type ErrorInfoLines = Seq[String]
  type Position = (Int, Int)

  abstract class Error {
    def getPos(): Position
    def generateErrorSpecifics(): (String, Seq[String])
    def prettyGenerateErrorSpecifics(fileName: String) = {
      val (head, specs) = generateErrorSpecifics()
      head + " in " + fileName + " " + getPos().toString + ":\n" +
        specs.foldLeft("")((acc, line) => acc + "  " + line + "\n")
    }
    def generateError(fileName: String) =
      prettyGenerateErrorSpecifics(fileName) + generateFileDisplay(fileName,
                                                                   getPos())
  }

  case class SyntaxError(pos: Position, lines: ErrorInfoLines) extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() = ("Syntax error", lines)
    override def generateError(fileName: String): String =
      prettyGenerateErrorSpecifics(fileName)
  }
  case class TypeError(pos: Position,
                       unexpectedType: String,
                       expectedTypes: List[String])
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Type error",
       Seq(
         "unexpected type: " + unexpectedType,
         "expected types: " + listToCommaSepString(expectedTypes)
       ))
    private def listToCommaSepString(list: List[String]): String = {
      val commaSep = list.foldLeft("")((acc, t) => acc + t + ", ")
      commaSep.substring(0, commaSep.length - 2)
    }
  }

  case class TypeMatchError(pos: Position,
                            identifier: String,
                            unexpectedType: String,
                            expectedType: String)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Type error for identifier " + identifier,
       Seq(
         "unexpected type: " + unexpectedType,
         "expected type: " + expectedType
       ))
  }

  case class BinaryOpAppTypeError(pos: Position, expectedTypes: List[String])
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Type error",
       Seq(
         "binary operator arguments should be of type: " + expectedTypes
           .mkString(", ")
       ))
  }

  case class ReadStatementError(pos: Position) extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Read statement error",
       Seq("cannot read into non-int or non-char types"))
  }

  case class RedeclaredFunctionError(pos: Position, identifier: String)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Function redeclaration error",
       Seq(
         "function " + identifier + " has already been declared in this scope"
       ))
  }

  case class UndeclaredFunctionError(pos: Position, identifier: String)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Undefined function error",
       Seq(
         "function " + identifier + " has not been defined"
       ))
  }

  case class PossibleMissingImportError(libName: String, pos: Position, identifier: String)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Possible missing import error",
       Seq(
         "function " + identifier + " has not been defined. Did you mean to import " + libName + "?"
       ))
  }

  case class NewPairError(pos: Position, message: String) extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() = ("New pair error", Seq(message))
  }

  case class ArrayArityError(pos: Position,
                             desiredArity: Int,
                             expectedArity: Int)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Array dimension error",
       Seq("expected dimension " + expectedArity + " but got " + desiredArity))

  }

  case class ArrayLiteralError(pos: Position, message: String) extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Array literal error", Seq(message))

  }

  case class UndeclaredVariableError(pos: Position, variableName: String)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      (
        "Scope error",
        Seq(
          "variable " + variableName + " has not been declared in this scope" + "\n")
      )
  }

  case class FunctionCallError(pos: Position, args: Int, required: Int)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Wrong number of function parameters",
       Seq(
         "expected " + required + " arguments but got " + args + " arguments"))
  }

  case class RedeclaredVariableError(pos: Position, variableName: String)
      extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Redeclared variable error",
       Seq("cannot redeclare variable " + variableName))
  }

  case class ReturnFromMainError(pos: Position) extends Error {
    override def getPos(): Position = pos
    override def generateErrorSpecifics() =
      ("Return from main error", Seq("cannot return from the main program"))
  }

  private def generateFileDisplay(fileName: String, pos: Position): String = {
    val (line, col) = pos
    val lines = readFile(fileName)
    return generateLineDisplay(line - 1, lines) + "\n" +
      generateLineDisplay(line, lines) + "\n" +
      generateColDisplay(col) + "\n" +
      generateLineDisplay(line + 1, lines)
  }

  private def generateLineDisplay(line: Int, lines: Seq[String]): String =
    "  |" + lines(line - 1)
  private def generateColDisplay(col: Int): String = " " * col + "^"

  private def readFile(filename: String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }
}
