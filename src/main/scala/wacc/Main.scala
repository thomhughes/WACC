package wacc

object Main {
  import java.io.File
  import wacc.Errors.Error
  import parsley.{Success, Failure}
  import wacc.Errors.SyntaxError
  import wacc.Parser.parse
  import wacc.AST.Program
  import wacc.Analyser.checkProgram
  import wacc.IR.buildIR

  def printErrors(ers: Seq[Error], fileName: String) =
    ers.foreach(error => {
      println(error.generateError(fileName))
    })

  def syntaxCheck(file: File): Either[SyntaxError, Program] = {
    parse(file).get match {
      case Success(program) => Right(program)
      case Failure(error)   => Left(error)
    }
  }

  def main(args: Array[String]): Unit = {
    val fileName = args.head
    syntaxCheck(new File(fileName)) match {
      case Left(syntaxError) => {
        println(syntaxError.generateError(fileName))
        sys.exit(100)
      }
      case Right(program) => {
        val (errors, symbolTable, functionTable) = checkProgram(program)
        if (!errors.isEmpty) {
          printErrors(errors, fileName)
          sys.exit(200)
        } else {
          sys.exit(0)
        }
      }
    }
  }
}
