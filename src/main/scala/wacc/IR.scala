package wacc

import scala.collection.mutable.{ListBuffer, Map}
import scala.language.implicitConversions
import wacc.Types._

// memmap maps from scope to no of bytes used
// every time we come accross a new variable,
// we increment memMap by 4 and map var to that
// address

object IR {

  // map from scope no to no of bytes
  import AST._
  import Types._

  def updateVar(identifier: Identifier, no: Int, bytes: Int)(
    implicit irProgram: IRProgram) = 
    irProgram.symbolTable.updateVar(identifier, no, bytes)

  def getScope()(implicit irProgram: IRProgram) = irProgram.symbolTable.scoper.getScope()

  def getNoBytes(saType: Type): Int = saType match {
    case IntType | ArrayType(_,_) | PairType(_, _) => 4
    case BoolType | CharType => 1
    case _ => throw new Exception("Unexpected LValue type")
  }

  def updateSymbolTable(identifier: Identifier, argType: Type)(implicit irProgram: IRProgram): Operand = {
    updateVar(identifier, getScope(), getNoBytes(argType))
    Var(identifier.name)
  }

  /* Evaluates lvalue and places result on top of the stack */
  def convertLValueToOperand(lvalue: LValue)(implicit irProgram: IRProgram): Operand = {
    lvalue match {
      case Identifier(x) => Var(x)
      case ArrayElem(id, indices) => ???
      case PairElem(index, lvalue) => ???
      case _ => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

  implicit def boolToInt(b: Boolean) = if (b) 1 else 0

  def nonBranchInstruction(op: BinaryOp): Instr =
    Instr(op match {
      case Plus => ADD
      case Minus => SUB
      case Mul => MUL
      case Div => DIV
      case Mod => MOD
      case And => AND
      case Or => ORR
    }, Some(R8), Some(R8), Some(R9))

  def branchInstruction(op: BinaryOp)(implicit irProgram: IRProgram) = {
    irProgram.instructions += Instr(CMP, Some(R8), Some(R8), Some(R9))
    irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(1)), None, op match {
      case Eq => EQ
      case Gt => GT
      case Ge => GE
      case Lt => LT
      case Le => LE
      case Neq => NE
    })
    irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(0)), None, op match {
      case Eq => NE
      case Gt => LE
      case Ge => LT
      case Lt => GE
      case Le => LE
      case Neq => EQ
    })
  }

  /* Evaluates expression and places result on top of the stack */
  def buildExpression(expr: Expression)(implicit irProgram: IRProgram): Unit = {
    implicit def boolToInt(i: Boolean): Int = if (i) 1 else 0
    expr match {
      case IntLiteral(value) => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(value)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case CharLiteral(char) => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(char.toInt)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case BoolLiteral(bool) => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(bool)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case StringLiteral(string) => {
        val label = LabelRef(".L.str" + irProgram.stringLiteralCounter)
        irProgram.stringLiteralCounter += 1
        Data(label, string) +=: irProgram.instructions
        irProgram.instructions += Instr(LDR, Some(R8), Some(label))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case PairLiteral => Imm(0)
      case Identifier(id) => {
        irProgram.instructions += Instr(LDR, Some(R8), Some(Var(id)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case BinaryOpApp(op, lexpr, rexpr) => {
        buildExpression(lexpr)
        buildExpression(rexpr)
        irProgram.instructions += Instr(POP, Some(R9))
        irProgram.instructions += Instr(POP, Some(R8))
        op match {
          case Plus | Minus | Mul | Div | Mod | And | Or => irProgram.instructions += nonBranchInstruction(op)
          case default => branchInstruction(op)
        }
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case UnaryOpApp(op, expr) => ???
      case ArrayElem(id, indices) => ???
      case default => throw new Exception("Invalid expression type")
    }
  }
  
  def buildArrayLiteral(args: List[Expression])(implicit irProgram: IRProgram): Unit = {
    /*
    we need to return something that can then be stored in a given variable
    the variable will store the address that has been malloced which will be
    at the top of the stack
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

  // def buildArrayReassignment(id: Identifier, indicies: List[Expression])(implicit irProgram: IRProgram) = {
  //   def arrLoadHelper(indicies: List[Expression]) = indicies match {
  //     case head :: next => head match {
  //       case IntLiteral => 
  //       case Identifier => 
  //       // case BinaryOpApp(op, lhs, rhs) => 
  //       case _ => throw new Exception("unexpected index type")
  //     }
  //   }
  // }

  /* Evaluates rvalue and places result on top of the stack */
  def buildRValue(rvalue: RValue)(implicit irProgram: IRProgram): Unit = {
    rvalue match {
      case e: Expression => buildExpression(e)
      case ArrayLiteral(args) => buildArrayLiteral(args)
      case FunctionCall(id, args) => ???
      case NewPair(e1, e2) => ???
      case PairElem(index, id) => ??? 
      case default => throw new Exception("Invalid rhs for assignment/declaration")
    }
  }

  def buildExit(expression: Expression)(implicit irProgram: IRProgram): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(BL, Some(LabelRef("exit")))
}

  def buildAssignment(lvalue: LValue, rvalue: RValue)(implicit irProgram: IRProgram): Unit = {
    buildRValue(rvalue)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(STR, Some(convertLValueToOperand(lvalue)), Some(R8))
  }

  def buildIf(condition: Expression, body1: List[Statement], body2: List[Statement]): Unit = {
  }
  
  def buildWhile(condition: Expression, body: List[Statement]): Unit = {
  }

  def getExpressionType(expression: Expression)(implicit irProgram: IRProgram): SAType = {
    expression match {
      case IntLiteral(_) => SAIntType
      case CharLiteral(_) => SACharType
      case BoolLiteral(_) => SABoolType
      case StringLiteral(_) => SAStringType
      case PairLiteral => SAPairType(SAAnyType, SAAnyType)
      case Identifier(id) => irProgram.symbolTable.lookupVarType(Identifier(id)((0, 0)))(List()) match {
        case Left(saType) => saType
        case Right(error) => throw new Exception("Invalid identifier type")
      }
      case ArrayElem(id, indices) => SAIntType
      case BinaryOpApp(_, lexpr, _) => getExpressionType(lexpr)
      case UnaryOpApp(op, expr) => op match {
        case Negation => SAIntType
        case Not => SABoolType
        case Len => SAIntType
        case Ord => SAIntType
        case Chr => SACharType
      }
      case default => throw new Exception("Invalid expression type")
    }
  }

  def buildPrint(expression: Expression)(implicit irProgram: IRProgram): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(PRINT(getExpressionType(expression)))
  }

  def buildFree(expression: Expression): Unit = {
  }

  def buildReturn(expression: Expression): Unit = {
  }
  
  def buildRead(lvalue: LValue): Unit = {
  }
  
  def enterScope()(implicit irProgram: IRProgram) = {
    irProgram.symbolTable.scoper.enterScope()
    irProgram.instructions += EnterScope(irProgram.symbolTable.scoper.getScope())
  }

  def exitScope()(implicit irProgram: IRProgram) = {
    irProgram.instructions += ExitScope(irProgram.symbolTable.scoper.getScope())
    irProgram.symbolTable.scoper.exitScope()
  }

  def buildBegin(statements: List[Statement])(implicit irProgram: IRProgram): Unit = {
    enterScope()
    statements.foreach(buildStatement(_))
    exitScope()
  }

  def buildPrintln(expression: Expression)(implicit irProgram: IRProgram): Unit = {
    buildPrint(expression)
    irProgram.instructions += Instr(PRINTLN)
  }

  def buildDeclaration(argType: Type, id: Identifier, rvalue: RValue)(
    implicit irProgram: IRProgram) = {
    updateVar(id, getScope(), getNoBytes(argType))
    buildAssignment(id, rvalue)
  }


  def buildStatement(statement: Statement)(implicit irProgram: IRProgram): Unit = {
    statement match {
        case ExitStatement(e) => buildExit(e)
        case AssignmentStatement(lvalue, rvalue) => buildAssignment(lvalue, rvalue)
        case DeclarationStatement(argType, id, rvalue) => buildDeclaration(argType, id, rvalue)
        case IfStatement(e, body1, body2) => buildIf(e, body1, body2)
        case SkipStatement => return
        case WhileStatement(e, body) => buildWhile(e, body)
        case PrintStatement(e) => buildPrint(e)
        case FreeStatement(e) => buildFree(e)
        case ReturnStatement(e) => buildReturn(e)
        case BeginStatement(statements) => buildBegin(statements)
        case ReadStatement(e) => buildRead(e)
        case PrintLnStatement(e) => buildPrintln(e)
        case _ => throw new Exception("Invalid statement type")
    }
  }

  def renameFunc(functionName: String) = "wacc_" + functionName

  def buildFunc(func: Func)(implicit irProgram: IRProgram): ListBuffer[IRType] = {
    irProgram.instructions += Label(renameFunc(func.identBinding.identifier.name))
    func.body.foreach(buildStatement(_))
    irProgram.instructions
  } 

  def buildFuncs(functions: List[Func])(implicit irProgram: IRProgram): ListBuffer[IRType] = {
    functions.foreach(buildFunc(_))
    irProgram.instructions
  }
  
  def buildIR(ast: Program, symbolTable: SymbolTable): ListBuffer[IRType] = {
    implicit val irProgram = IRProgram(ListBuffer(), 0, symbolTable)
    buildFuncs(ast.functions)
    ast.statements.foreach(buildStatement(_))
    irProgram.instructions
  }
}
