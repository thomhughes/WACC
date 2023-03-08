package wacc

object Main {
  import java.io.File
  import wacc.Errors.Error
  import parsley.{Success, Failure}
  import wacc.Errors.SyntaxError
  import wacc.Parser.parse
  import wacc.Analyser.checkProgram
  import wacc.IR.buildIR
  import wacc.IRToAssemblyConverter.convertAssembly
  import wacc.Peephole.peepholeOptimisation
  import scala.collection.mutable.Map
  import wacc.AST._
  import scala.collection.mutable.Set

  val lengthOfSmallFunction = 6
  val maxNoCallsOfFunction = 6

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

  def parseLibraries(): Map[String, List[(String, (Type, List[Type]))]] = {
    Map(("array", List(
      ("contains", (BoolType, List(ArrayType(IntType, 1)(0,0), IntType))),
      ("reverse", (IntType, List(ArrayType(IntType, 1)(0,0)))),
      ("sum", (IntType, List(ArrayType(IntType, 1)(0,0)))),
      ("min", (IntType, List(ArrayType(IntType, 1)(0,0)))),
      ("max", (IntType, List(ArrayType(IntType, 1)(0,0)))),
      ("qsort", (IntType, List(ArrayType(IntType, 1)(0,0)))))),
    ("string", List(
      ("strcat", (StringType, List(StringType, StringType))),
      ("strlen", (IntType, List(StringType))),
      ("atoi", (IntType, (List(StringType)))))),
    ("math", List(
      ("abs", (IntType, List(IntType))),
      ("pow", (IntType, List(IntType, IntType))),
      ("rand", (IntType, List())),
      ("srand", (IntType, (List(IntType))))))
    )
  }

  def reverseLibToFuncMap(map: Map[String, List[(String, (Type, List[Type]))]]): Map[String, String] = {
    val funcToLibMap: Map[String, String] = Map()
    map.foreach((x: (String, List[(String, (Type, List[Type]))])) => {
      x._2.foreach((y: (String, (Type, List[Type]))) => {
        funcToLibMap += ((y._1, x._1))
      })
    })
    funcToLibMap
  }

  def getNoFunctionCalls(program: Program, function: Func): Int = {
      program.functions.count(func => func.identBinding.identifier == function.identBinding.identifier)
  }

  def getInlinedFunctions(program: Program): (Set[String], Map[String, (List[Parameter], List[Statement])]) = {
    val functionsToBeInlined: Set[String] = Set()
    val inlinedFunctionsAndBodies: Map[String, (List[Parameter], List[Statement])] = Map()
    program.functions.foreach(
      func => {
        if ((func.body.length < lengthOfSmallFunction || getNoFunctionCalls(program, func) < maxNoCallsOfFunction) && !(
          !(func.body.filter(
            stmt => stmt match {
                case AssignmentStatement(_,rvalue) => rvalue match {
                    case FunctionCall(func.identBinding.identifier,_) => true
                    case default => false
                }
                case DeclarationStatement(_,_,rvalue) => rvalue match {
                    case FunctionCall(func.identBinding.identifier,_) => true
                    case default => false
                }
                case default => false
            }
        ).isEmpty)
        )) {
          functionsToBeInlined += func.identBinding.identifier.name
          inlinedFunctionsAndBodies += ((func.identBinding.identifier.name, (func.params, func.body)))
        }
      }
    )
    (functionsToBeInlined, inlinedFunctionsAndBodies)
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
        implicit val (inlinedFunctions, inlinedFunctionsAndBodies) = getInlinedFunctions(program)
        implicit val libToFuncMap: Map[String, List[(String, (Type, List[Type]))]] = parseLibraries()
        implicit val funcToLibMap = reverseLibToFuncMap(libToFuncMap)
        val (errors, symbolTable, functionTable) = checkProgram(program)
        if (!errors.isEmpty) {
          printErrors(errors, fileName)
          sys.exit(200)
        } else {
          val instructions = buildIR(program, symbolTable)
          val assembly = convertAssembly(peepholeOptimisation(instructions))
          val assemblyFileName = getAssemblyFileName(fileName)
          printToFile(assembly, assemblyFileName)
          sys.exit(0)
        }
      }
    }
  }
}
