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
  import wacc.IRToAssemblyConverter.convertAssembly

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

  def assertInputFilenameIsValid(fileName: String) = {
    if (!fileName.endsWith(".wacc")) {
      throw new Exception(
        "InvalidFileNameError: Filename input into compiler does not end with .wacc"
      )
    }
  }

  def getAssemblyFileName(inputFilePath: String) =
    inputFilePath.substring(
      inputFilePath.lastIndexOf("/") + 1,
      inputFilePath.length() - ".wacc".length()
    ) + ".s"

  def printToFile(fileContents: String, fileName: String) = {
    val p = new java.io.PrintWriter(new File(fileName))
    try { p.println(fileContents) } finally { p.close() }
  }

  def main(args: Array[String]): Unit = {
    val fileName = args.head
    assertInputFilenameIsValid(fileName)
    syntaxCheck(new File(fileName)) match {
      case Left(syntaxError) => {
        println(syntaxError.generateError(fileName))
        sys.exit(100)
      }
      case Right(program) => {
        val (errors, symbolTable, functionTable, updatedProgram) = checkProgram(program)
        if (!errors.isEmpty) {
          printErrors(errors, fileName)
          sys.exit(200)
        } else {
          val (instructions, updatedSymbolTable) = buildIR(updatedProgram, symbolTable)
          val assembly = convertAssembly(instructions, updatedSymbolTable)
          val assemblyFileName = getAssemblyFileName(fileName)
          printToFile(assembly, assemblyFileName)
          sys.exit(0)
        }
      }
    }
  }
}
