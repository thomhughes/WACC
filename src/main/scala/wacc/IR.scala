package wacc

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object IR {
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

  // TODO: Finish implementing this
  def convertRValueToOperand(rvalue: RValue): Operand = {
    rvalue match {
      case IntLiteral(value) => Imm(value)
      case CharLiteral(char) => Imm(char.toInt)
      case BoolLiteral(bool) => Imm(bool)
      case StringLiteral(string) => ???
    //   case BinaryOpApp(op, lhs, rhs) => 
    // cases for other literals n shit 
      case default => throw new Exception("Invalid rhs for assignment/declaration")
    }
  }

  def buildExit(expression: Expression)(implicit ir: ListBuffer[IRType]): Unit = {
    expression match {
      case IntLiteral(exitCode) => ir += Instr(MOV, Some(R0), Some(Imm(exitCode)))
      case Identifier(varName) => {
        // TODO: perform lookup and append
      }
      case _ => throw new Exception("Exit code is non-integer")
    }
    ir += Instr(BL, Some(LabelRef("exit")))
}

  def buildAssignment(lvalue: LValue, rvalue: RValue)(implicit ir: ListBuffer[IRType]): Unit = 
    ir += Instr(STR, Some(convertLValueToOperand(lvalue)), Some(convertRValueToOperand(rvalue)))

  def buildIf(condition: Expression, body1: List[Statement], body2: List[Statement])(implicit ir: ListBuffer[IRType]): Unit = {
  }
  
  def buildWhile(condition: Expression, body: List[Statement])(implicit ir: ListBuffer[IRType]): Unit = {
  }

  def buildPrint(expression: Expression)(implicit ir: ListBuffer[IRType]): Unit = {
  }

  def buildFree(expression: Expression)(implicit ir: ListBuffer[IRType]): Unit = {
  }

  def buildReturn(expression: Expression)(implicit ir: ListBuffer[IRType]): Unit = {
  }
  
  def buildRead(lvalue: LValue)(implicit ir: ListBuffer[IRType]): Unit = {
  }

  
  def enterScope()(implicit ir: ListBuffer[IRType], scoper: Scoper) = {
    scoper.enterScope()
    ir += EnterScope(scoper.getScope())
  }

  def exitScope()(implicit ir: ListBuffer[IRType], scoper: Scoper) = {
    ir += ExitScope(scoper.getScope())
    scoper.exitScope()
  }

  def buildBegin(statements: List[Statement])(implicit ir: ListBuffer[IRType], scoper: Scoper): Unit = {
    enterScope()
    statements.foreach(buildStatement(_))
    exitScope()
  }

  def buildPrintln(expression: Expression)(implicit ir: ListBuffer[IRType]): Unit = {
  }

  def buildStatement(statement: Statement)(implicit ir: ListBuffer[IRType], scoper: Scoper): Unit = {
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

  def buildFunc(func: Func)(implicit ir: ListBuffer[IRType], scoper: Scoper): ListBuffer[IRType] = {
    ir += Label(renameFunc(func.identBinding.identifier.name))
    
    // collector(func.body, buildStatement)
    // val allIR = ListBuffer[IRType]()
    func.body.foreach(buildStatement(_))
    ir
  } 

  def buildFuncs(functions: List[Func])(implicit ir: ListBuffer[IRType], scoper: Scoper): ListBuffer[IRType] = {
    // collector(functions, buildFunc)
    functions.foreach(buildFunc(_))
    ir
  }
  
  def buildIR(ast: Program): ListBuffer[IRType] = {
    implicit val ir = ListBuffer[IRType]()
    implicit val scoper = new Scoper()
    buildFuncs(ast.functions)
    ast.statements.foreach(buildStatement(_))
    ir
  }
}
