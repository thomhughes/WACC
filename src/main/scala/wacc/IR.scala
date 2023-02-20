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
  val ir = ListBuffer[IRType]()
  val scoper = new Scoper()
  import AST._

  // TODO: Finish implementing this
  def convertLValueToOperand(lvalue: LValue): Operand = {
    lvalue match {
      case Identifier(name) => Var(name)
      case ArrayElem(id, indices) => ???
      case PairElem(index, lvalue) => ???
      case default => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

  implicit def boolToInt(b: Boolean) = if (b) 1 else 0


  def convertExpressionToOperand(expr: Expression): Operand = expr match {
    case IntLiteral(value) => {
      ir += Instr(PUSH, Some(Imm(value)))
      Imm(value)
    }
    case CharLiteral(char) => {
      ir += Instr(PUSH, Some(Imm(char.toInt)))
      Imm(char.toInt)
    }
    case BoolLiteral(bool) => {
      ir += Instr(PUSH, Some(Imm(bool)))
      Imm(bool)
    }
    case StringLiteral(string) => ???
    case PairLiteral => Imm(0)
    case Identifier(id) => {
      ir += Instr(PUSH, Some(Var(id)))
      Var(id)
    }
    case BinaryOpApp(op, lexpr, rexpr) => op match {
      case Plus => {
        ir += Instr(MOV, Some(R8), Some(convertExpressionToOperand(lexpr)))
        ir += Instr(MOV, Some(R9), Some(convertExpressionToOperand(rexpr)))
        // pop two operands for use in addition, push in all cases
        ir += Instr(POP, Some(R9))
        ir += Instr(POP, Some(R8))
        ir += Instr(ADD, Some(R8), Some(R8), Some(R9))
        ir += Instr(PUSH, Some(R8))
        R8
      } 
    }
    case UnaryOpApp(op, expr) => ???
  }
  

  def buildArrayLiteral(args: List[Expression]): Operand = {
    /*
    we need to return something that can then be stored in a given variable
    the variable will store the address that has been malloced which will be in 
    R12.
    length offset is used to store the length of each array before each elem
    */
    val lengthOffset = 1
    val bytesPerInt = 4
    ir += Instr(MALLOC, Some(Imm(args.length + lengthOffset)))
    ir += Instr(MOV, Some(R0), Some(R12))
    ir += Instr(ADD, Some(R12), Some(R12), Some(Imm(bytesPerInt * lengthOffset)))
    ir += Instr(STR, Some(R12), Some(ArrayToStore(args)))
    R12
  }

  // TODO: Finish implementing this
  def convertRValueToOperand(rvalue: RValue): Operand = {
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

  def buildExit(expression: Expression): Unit = {
    expression match {
      case IntLiteral(exitCode) => ir += Instr(MOV, Some(R0), Some(Imm(exitCode)))
      case Identifier(varName) => {
        // TODO: perform lookup and append
      }
      case _ => throw new Exception("Exit code is non-integer")
    }
    ir += Instr(BL, Some(LabelRef("exit")))
}

  def buildAssignment(lvalue: LValue, rvalue: RValue): Unit = 
    ir += Instr(STR, Some(convertLValueToOperand(lvalue)), Some(convertRValueToOperand(rvalue)))

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

  
  def enterScope() = {
    scoper.enterScope()
    ir += EnterScope(scoper.getScope())
  }

  def exitScope() = {
    ir += ExitScope(scoper.getScope())
    scoper.exitScope()
  }

  def buildBegin(statements: List[Statement]): Unit = {
    enterScope()
    statements.foreach(buildStatement(_))
    exitScope()
  }

  def buildPrintln(expression: Expression): Unit = {
  }

  def buildStatement(statement: Statement): Unit = {
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

  def buildFunc(func: Func): ListBuffer[IRType] = {
    ir += Label(renameFunc(func.identBinding.identifier.name))
    
    // collector(func.body, buildStatement)
    // val allIR = ListBuffer[IRType]()
    func.body.foreach(buildStatement(_))
    ir
  } 

  def buildFuncs(functions: List[Func]): ListBuffer[IRType] = {
    // collector(functions, buildFunc)
    functions.foreach(buildFunc(_))
    ir
  }
  
  def buildIR(ast: Program, symbolTable: SymbolTable): ListBuffer[IRType] = {
    buildFuncs(ast.functions)
    ast.statements.foreach(buildStatement(_))
    ir
  }
}
