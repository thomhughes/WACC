package wacc

object Main {
  import java.io.File
  import wacc.Errors.Error
  import parsley.{Success, Failure}
  import wacc.Errors.SyntaxError
  import wacc.Parser.parse
  import wacc.Analyser.{checkProgram, renameFunctionRule}
  import wacc.IR.buildIR
  import wacc.IRToAssemblyConverter.convertAssembly
  import wacc.Peephole.peepholeOptimisation
  import scala.collection.mutable.Map
  import wacc.AST._

  val successExitCode = 0
  val syntaxErrorExitCode = 100
  val semanticErrorExitCode = 200

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

  def parseAndAddToMap(file: String)(implicit libFuncMap: Map[String, List[Func]]) = {
    (syntaxCheck(new File(file)): @unchecked) match {
      case Right(value) => {
        libFuncMap += ((file, value.functions))
      }
    }
  }

  def parseLibraries(): Map[String, List[(String, List[(Type, List[Type])])]] = {
    Map(("array", List(
      ("contains", List((BoolType, List(ArrayType(IntType, 1)(0,0), IntType)), (BoolType, List(ArrayType(CharType, 1)(0,0), CharType)))),
      ("reverse", List((IntType, List(ArrayType(IntType, 1)(0,0))), (IntType, List(ArrayType(CharType, 1)(0,0))))),
      ("sum", List((IntType, List(ArrayType(IntType, 1)(0,0))))),
      ("min", List((IntType, List(ArrayType(IntType, 1)(0,0))), (CharType, List(ArrayType(CharType, 1)(0,0))))),
      ("max", List((IntType, List(ArrayType(IntType, 1)(0,0))), (CharType, List(ArrayType(CharType, 1)(0, 0))))),
      ("qsort", List((IntType, List(ArrayType(IntType, 1)(0,0))), (IntType, List(ArrayType(CharType, 1)(0,0))))))),
    ("string", List(
      ("strcat", List((StringType, List(StringType, StringType)))),
      ("strlen", List((IntType, List(StringType)))),
      ("atoi", List((IntType, (List(StringType))))))),
    ("math", List(
      ("abs", List((IntType, List(IntType)))),
      ("pow", List((IntType, List(IntType, IntType)))),
      ("rand", List((IntType, List()))),
      ("srand", List((IntType, (List(IntType)))))))
    )
  }

  def reverseLibToFuncMap(map: Map[String, List[(String, List[(Type, List[Type])])]]): Map[String, String] = {
    val funcToLibMap: Map[String, String] = Map()
    map.foreach((x: (String, List[(String, List[(Type, List[Type])])])) => {
      x._2.foreach((y: (String, List[(Type, List[Type])])) => {
        var counter = 0
        y._2.foreach((z: (Type, List[Type])) => {
          funcToLibMap += (renameFunctionRule(y._1, counter) -> x._1)
          counter += 1
        })
      })
    })
    funcToLibMap
  }

  def main(args: Array[String]): Unit = {
    val fileName = args.head
    assertInputFilenameIsValid(fileName)
    val parseOutput = syntaxCheck(new File(fileName))
    parseOutput match {
      case Left(syntaxError) => {
        println(syntaxError.generateError(fileName))
        sys.exit(syntaxErrorExitCode)
      }
      case Right(program) => {
        implicit val libToFuncMap: Map[String, List[(String, List[(Type, List[Type])])]] = parseLibraries()
        implicit val funcToLibMap = reverseLibToFuncMap(libToFuncMap)
        val (errors, symbolTable, functionTable, updatedProgram) = checkProgram(program)
        if (!errors.isEmpty) {
          printErrors(errors, fileName)
          sys.exit(semanticErrorExitCode)
        } else {
          val instructions = buildIR(updatedProgram, symbolTable)
          val assembly = convertAssembly(peepholeOptimisation(instructions))
          val assemblyFileName = getAssemblyFileName(fileName)
          printToFile(assembly, assemblyFileName)
          sys.exit(successExitCode)
        }
      }
    }
  }
}
