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
<<<<<<< HEAD
      // case _: Error => "unknown error"
=======
      case _: Error => error.toString()
>>>>>>> semantic_errors
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

  // def semanticCheck(program: Program): Option[Seq[Error]] = {
  //   checkProgram(program) match {
  //     case true => None
  //     case false => Some(Seq(SyntaxError((2, 3), Seq("semantic error"))))
  //   }
  // }

  def main(args: Array[String]): Unit = {
    syntaxCheck(new File(args.head)) match {
      case Left(syntaxError) => {
        println(generateError(syntaxError))
        sys.exit(100)
      }
      case Right(program) => {
        val errors = checkProgram(program)
        if (!errors.isEmpty) {
          printErrors(errors)
          sys.exit(200)
        } else {
          sys.exit(0)
        }
      }
    }
  }
}
 
