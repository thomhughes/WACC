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
  import wacc.Peephole.peepholeOptimisation
  import java.nio.file.{Files, Paths}
  import scala.jdk.CollectionConverters._
  import wacc.AST.Func
  import wacc.AST.Import
  import scala.collection.mutable.Map

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

  def parseLibraries() = {
    implicit val libFuncMap: Map[String, List[Func]] = Map()
    Files
      .walk(Paths.get("wacc_examples/libraries"))
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .foreach((x) => parseAndAddToMap(x.toString()))
    libFuncMap
  }

  def addDependency(importRef: Import)(implicit newFuncs: List[Func], funcMap: Map[String, List[Func]]) = {
    newFuncs :+ funcMap.get(importRef.importName)
  }

  def reverseLibToFuncMap(map: Map[String, List[Func]]): Map[String, String] = {
    val funcToLibMap: Map[String, String] = Map()
    map.foreach((x: (String, List[Func])) => x._2.foreach((func: Func) => {
      funcToLibMap += ((func.identBinding.identifier.name, x._1))
    }))
    funcToLibMap
  }

  def main(args: Array[String]): Unit = {
    val fileName = args.head
    assertInputFilenameIsValid(fileName)
    val parseOutput = syntaxCheck(new File(fileName))
    parseOutput match {
      case Left(syntaxError) => {
        println(syntaxError.generateError(fileName))
        sys.exit(100)
      }
      case Right(program) => {
        implicit val libToFuncMap = parseLibraries()
        implicit val funcToLibMap = reverseLibToFuncMap(libToFuncMap)
        implicit val newFuncs: List[Func] = List()
        program.imports.foreach(addDependency)
        val newProgram = Program(List(), newFuncs ++ program.functions, program.statements)(0,0)
        val (errors, symbolTable, functionTable) = checkProgram(newProgram)
        if (!errors.isEmpty) {
          printErrors(errors, fileName)
          sys.exit(200)
        } else {
          val instructions = buildIR(newProgram, symbolTable)
          val assembly = convertAssembly(peepholeOptimisation(instructions))
          val assemblyFileName = getAssemblyFileName(fileName)
          printToFile(assembly, assemblyFileName)
          sys.exit(0)
        }
      }
    }
  }
}
