package wacc

object Main {
  import java.io.File
  import wacc.Errors.Error
  import parsley.{Success, Failure}
  import wacc.Errors.SyntaxError
  import wacc.Parser.parse
  import wacc.AST.Program
  import wacc.Analyser.checkProgram

  def generateError(error: Error): String = {
    error match {
      case SyntaxError(pos, lines) => lines.toString() 
      case _: Error => "unknown error"
    }
  }

  def printErrors(ers: Seq[Error]) = ers.foreach(error => {
    println(generateError(error))
  })

  def syntaxCheck(file: File): Either[SyntaxError, Program] = {
    parse(file).get match {
      case Success(program) => Right(program)
      case Failure(error) => Left(error)
    }
  }

  def semanticCheck(program: Program): Option[Seq[Error]] = {
    checkProgram(program) match {
      case true => None
      case false => Some(Seq(SyntaxError(Errors.Position(2, 3), Seq("semantic error"))))
    }
  }

  def main(args: Array[String]): Unit = {
    syntaxCheck(new File(args.head)) match {
      case Left(syntaxError) => {
        println(generateError(syntaxError))
        sys.exit(100)
      }
      case Right(program) => {
        semanticCheck(program) match {
          case Some(errors) => {
            printErrors(errors)
            sys.exit(200)
          }
          case None => sys.exit(0)
        }
      }
    }
  }
}
 
