package wacc

import scala.collection.mutable.{ListBuffer, Map}
import scala.language.implicitConversions

// int[] arr = [1, 2, 3]
// need to know length of the array to proceed.
// dont care for arity
// symbol table will have location for nested arrays
// maybe an array table?

object IR {

  val memMap = Map[Int, Int]()
  val scoper = new Scoper()
  import AST._

  /* Evaluates lvalue and places result on top of the stack */
  def convertLValueToOperand(lvalue: LValue): Operand = {
    lvalue match {
      case Identifier(name) => Var(name)
      case ArrayElem(id, indices) => ???
      case PairElem(index, lvalue) => ???
      case default => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

  implicit def boolToInt(b: Boolean) = if (b) 1 else 0

  /* Evaluates expression and places result on top of the stack */
  def convertExpressionToOperand(expr: Expression)(implicit irProgram: IRProgram): Unit = expr match {
    case IntLiteral(value) => irProgram.instructions += Instr(PUSH, Some(Imm(value)))
    case CharLiteral(char) => irProgram.instructions += Instr(PUSH, Some(Imm(char.toInt)))
    case BoolLiteral(bool) => irProgram.instructions += Instr(PUSH, Some(Imm(bool)))
    case StringLiteral(string) => ???
    case PairLiteral => Imm(0)
    case Identifier(id) => irProgram.instructions += Instr(PUSH, Some(Var(id)))
    case BinaryOpApp(op, lexpr, rexpr) => op match {
      case Plus => {
        convertExpressionToOperand(lexpr)
        convertExpressionToOperand(rexpr)
        // pop two operands for use in addition, push in all cases
        irProgram.instructions += Instr(POP, Some(R9))
        irProgram.instructions += Instr(POP, Some(R8))
        irProgram.instructions += Instr(ADD, Some(R8), Some(R8), Some(R9))
        irProgram.instructions += Instr(PUSH, Some(R8))
      } 
    }
    case UnaryOpApp(op, expr) => ???
  }
  
  def buildArrayLiteral(args: List[Expression])(implicit irProgram: IRProgram): Unit = {
    /*
    we need to return something that can then be stored in a given variable
    the variable will store the address that has been malloced which will be in 
    R12.
    length offset is used to store the length of each array before each elem
    */
    val lengthOffset = 1
    val bytesPerInt = 4
    irProgram.instructions += Instr(MALLOC, Some(Imm(args.length + lengthOffset)))
    irProgram.instructions += Instr(MOV, Some(R0), Some(R12))
    irProgram.instructions += Instr(ADD, Some(R12), Some(R12), Some(Imm(bytesPerInt * lengthOffset)))
    irProgram.instructions += Instr(STR, Some(R12), Some(ArrayToStore(args)))
    irProgram.instructions += Instr(PUSH, Some(R12))
  }

  /* Evaluates rvalue and places result on top of the stack */
  def convertRValueToOperand(rvalue: RValue)(implicit irProgram: IRProgram): Unit = {
    rvalue match {
      case e: Expression => convertExpressionToOperand(e)
      case ArrayElem(id, indices) => ???
      case ArrayLiteral(args) => buildArrayLiteral(args)
      case FunctionCall(id, args) => ???
      case NewPair(e1, e2) => ???
      case PairElem(index, id) => ??? 
    //   case BinaryOpApp(op, lhs, rhs) => 
    // cases for other literals n shit 
      case default => throw new Exception("Invalid rhs for assignment/declaration")
    }
  }

  def buildExit(expression: Expression)(implicit irProgram: IRProgram): Unit = {
    expression match {
      case IntLiteral(exitCode) => irProgram.instructions += Instr(MOV, Some(R0), Some(Imm(exitCode)))
      case Identifier(varName) => {
        // TODO: perform lookup and append
      }
      case _ => throw new Exception("Exit code is non-integer")
    }
    irProgram.instructions += Instr(BL, Some(LabelRef("exit")))
}

  def buildAssignment(lvalue: LValue, rvalue: RValue)(implicit irProgram: IRProgram): Unit = {
    convertRValueToOperand(rvalue)
    irProgram.instructions += Instr(POP, Some(R9))
    irProgram.instructions += Instr(STR, Some(convertLValueToOperand(lvalue)), Some(R9))
  }

  def buildIf(condition: Expression, body1: List[Statement], body2: List[Statement]): Unit = {
  }
  
  def buildWhile(condition: Expression, body: List[Statement]): Unit = {
  }

  def buildPrint(expression: Expression): Unit = {
  }

  def buildFree(expression: Expression): Unit = {
  }

  def buildReturn(expression: Expression): Unit = {
  }
  
  def buildRead(lvalue: LValue): Unit = {
  }

  
  def enterScope()(implicit irProgram: IRProgram) = {
    scoper.enterScope()
    irProgram.instructions += EnterScope(scoper.getScope())
  }

  def exitScope()(implicit irProgram: IRProgram) = {
    irProgram.instructions += ExitScope(scoper.getScope())
    scoper.exitScope()
  }

  def buildBegin(statements: List[Statement])(implicit irProgram: IRProgram): Unit = {
    enterScope()
    statements.foreach(buildStatement(_))
    exitScope()
  }

  def buildPrintln(expression: Expression): Unit = {
  }

  def buildStatement(statement: Statement)(implicit irProgram: IRProgram): Unit = {
    statement match {
        case ExitStatement(e) => buildExit(e)
        case AssignmentStatement(lvalue, rvalue) => buildAssignment(lvalue, rvalue)
        case DeclarationStatement(_, id, rvalue) => buildAssignment(id, rvalue)
        case IfStatement(e, body1, body2) => buildIf(e, body1, body2)
        case SkipStatement => return
        case WhileStatement(e, body) => buildWhile(e, body)
        case PrintStatement(e) => buildPrint(e)
        case FreeStatement(e) => buildFree(e)
        case ReturnStatement(e) => buildReturn(e)
        case BeginStatement(statements) => buildBegin(statements)
        case ReadStatement(e) => buildRead(e)
        case PrintLnStatement(e) => buildPrintln(e)
        case default => throw new Exception("Invalid statement type")
    }
  }

  // def collector[A, B](list: List[A], func: A => ListBuffer[B])(implicit acc: ListBuffer[B]): ListBuffer[B] = list match {
  //   case head :: next => collector(next, func)(acc ++ func(head))
  //   case Nil          => acc
  // }

  def renameFunc(functionName: String) = "wacc_" + functionName

  def buildFunc(func: Func)(implicit irProgram: IRProgram): ListBuffer[IRType] = {
    irProgram.instructions += Label(renameFunc(func.identBinding.identifier.name))
    
    // collector(func.body, buildStatement)
    // val allIR = ListBuffer[IRType]()
    func.body.foreach(buildStatement(_))
    irProgram.instructions
  } 

  def buildFuncs(functions: List[Func])(implicit irProgram: IRProgram): ListBuffer[IRType] = {
    // collector(functions, buildFunc)
    functions.foreach(buildFunc(_))
    irProgram.instructions
  }
  
  def buildIR(ast: Program, symbolTable: SymbolTable): ListBuffer[IRType] = {
    implicit val dataMap: Map[Identifier, LabelRef] = Map()
    implicit val irProgram = IRProgram(ListBuffer(), dataMap)
    buildFuncs(ast.functions)
    ast.statements.foreach(buildStatement(_))
    irProgram.instructions
  }
}
