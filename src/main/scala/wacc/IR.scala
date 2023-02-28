package wacc

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

import wacc.SymbolTable
// memmap maps from scope to no of bytes used
// every time we come accross a new variable,
// we increment memMap by 4 and map var to that
// address

object IR {

  // map from scope no to no of bytes
  import AST._
  import Types._
  import Analyser.convertSyntaxToTypeSys

  val getNoBytes: (SAType => Int) = _ match {
    case SAIntType | SAArrayType(_, _) | SAPairType(_, _) => 4
    case SABoolType | SACharType                          => 1
    case _ => throw new Exception("Unexpected LValue type")
  }

  def getScope()(implicit irProgram: IRProgram, funcName: String) =
    irProgram.symbolTable.getScope()

  /* Evaluates lvalue and places result on top of the stack */
  def convertLValueToOperand(
      lvalue: LValue
  )(implicit irProgram: IRProgram): Option[Operand] = {
    lvalue match {
      case Identifier(x)           => Some(Var(x))
      case ArrayElem(id, indices)  => buildArrayReassignment(id, indices)
      case PairElem(index, lvalue) => ???
      case _ => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

  implicit def boolToInt(b: Boolean) = if (b) 1 else 0

  def nonCompareInstruction(op: BinaryOp): Instr =
    Instr(
      (op: @unchecked) match {
        case Plus  => ADD
        case Minus => SUB
        case Mul   => MUL
        case Div   => DIV
        case Mod   => MOD
        case And   => AND
        case Or    => ORR
      },
      Some(R8),
      Some(R8),
      Some(R9)
    )

  def compareInstruction(op: BinaryOp)(implicit irProgram: IRProgram) = {
    irProgram.instructions += Instr(CMP, Some(R8), Some(R8), Some(R9))
    irProgram.instructions += Instr(
      MOV,
      Some(R8),
      Some(Imm(1)),
      None,
      (op: @unchecked) match {
        case Eq  => EQ
        case Gt  => GT
        case Ge  => GE
        case Lt  => LT
        case Le  => LE
        case Neq => NE
      }
    )
    irProgram.instructions += Instr(
      MOV,
      Some(R8),
      Some(Imm(0)),
      None,
      (op: @unchecked) match {
        case Eq  => NE
        case Gt  => LE
        case Ge  => LT
        case Lt  => GE
        case Le  => LE
        case Neq => EQ
      }
    )
  }

  def modifyingUnaryOp(op: UnaryOp)(implicit irProgram: IRProgram): Unit = {
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += ((op: @unchecked) match {
      case Not      => Instr(MVN, Some(R8), Some(R8))
      case Negation => Instr(RSB, Some(R8), Some(Imm(0)))
      case Len      => Instr(LEN, Some(R8))
    })
    irProgram.instructions += Instr(PUSH, Some(R8))
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
        irProgram.instructions += Data(label, string)
        irProgram.stringLiteralCounter += 1
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
          case Plus | Minus | Mul | Div | Mod | And | Or =>
            irProgram.instructions += nonCompareInstruction(op)
          case default => compareInstruction(op)
        }
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case UnaryOpApp(op, expr) => {
        buildExpression(expr)
        op match {
          case Not | Negation | Len => modifyingUnaryOp(op)
          case default              => ()
        }
      }
      case ArrayElem(id, indices) => buildArrayAccess(id, indices)
      case default => throw new Exception("Invalid expression type")
    }
  }

  def buildArrayLiteral(args: List[Expression], argType: SAType)(implicit
      irProgram: IRProgram
  ): Unit = {
    /*
    we need to return something that can then be stored in a given variable
    the variable will store the address that has been malloced which will be
    at the top of the stack
    length offset is used to store the length of each array before each elem
     */
    val lengthOffset = 1
    val bytesPerInt = 4
    val elementSize = getNoBytes(argType)
    irProgram.instructions += Instr(
      MALLOC,
      Some(Imm(args.length * elementSize + lengthOffset))
    )
    irProgram.instructions += Instr(MOV, Some(R12), Some(R0))
    irProgram.instructions += Instr(
      ADD,
      Some(R12),
      Some(R12),
      Some(Imm(bytesPerInt * lengthOffset))
    )
    irProgram.instructions += Instr(STR, Some(R12), Some(ArrayToStore(args)))
    irProgram.instructions += Instr(PUSH, Some(R12))
  }

  // When this is called, arg to be stored is in R8
  def buildArrayReassignment(id: Identifier, indices: List[Expression])(implicit
      irProgram: IRProgram
  ): Option[Operand] = {
    buildArrLoad(id, indices)
    // last index is now on top of stack
    // From ArrLoad and buildRValue, we know stack
    // has index, then pointer to array, then arg to be stored
    irProgram.instructions += Instr(POP, Some(R10))
    irProgram.instructions += Instr(POP, Some(R3))
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(ARRSTORE)
    None
  }

  def buildArrayAccess(id: Identifier, indices: List[Expression])(implicit
      irProgram: IRProgram
  ): Unit = {
    buildArrLoad(id, indices)
    // last index is now top of stack
    irProgram.instructions += Instr(POP, Some(R10))
    irProgram.instructions += Instr(POP, Some(R3))
    irProgram.instructions += Instr(ARRLOAD)
    // result is loaded in R3, we push to top of stack as per convention
    irProgram.instructions += Instr(PUSH, Some(R3))
  }

  // after this function is called, the pointer to the list will be on the top of the stack
  def buildArrLoad(id: Identifier, indices: List[Expression])(implicit
      irProgram: IRProgram
  ): Unit = {
    irProgram.instructions += Instr(LDR, Some(Var(id.name)), Some(R9))
    irProgram.instructions += Instr(PUSH, Some(R9))
    (indices: @unchecked) match {
      case head :: Nil =>
        buildExpression(head) // this will put the index on the top of the stack
      case head :: next => {
        // Load the current array pointer into R3
        irProgram.instructions += Instr(POP, Some(R3))
        buildExpression(head)
        // arg is now on top of stack: we will pop and proceed
        irProgram.instructions += Instr(POP, Some(R10))
        irProgram.instructions += Instr(ARRLOAD)
        irProgram.instructions += Instr(PUSH, Some(R3))
        buildArrLoad(id, next)
      }
    }
  }

  def buildPairElement(element: Expression, noBytes: Int)(implicit
      irProgram: IRProgram
  ) = {
    irProgram.instructions += Instr(MOV, Some(R0), Some(Imm(noBytes)))
    irProgram.instructions += Instr(MALLOC)
    irProgram.instructions += Instr(MOV, Some(R12), Some(R0))
    buildExpression(element)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(STR, Some(R8), Some(R12))
    irProgram.instructions += Instr(PUSH, Some(R12))
  }

  def buildNewPair(e1: Expression, e2: Expression, argType: SAType)(implicit
      irProgram: IRProgram
  ) = {
    argType match {
      case SAPairType(firstType, secondType) => {
        val noFirstBytes = getNoBytes(firstType)
        val noSecondBytes = getNoBytes(secondType)
        buildPairElement(e1, noFirstBytes)
        buildPairElement(e2, noSecondBytes)
        irProgram.instructions += Instr(
          MOV,
          Some(R0),
          Some(Imm(noFirstBytes + noSecondBytes))
        )
        irProgram.instructions += Instr(MALLOC)
        irProgram.instructions += Instr(MOV, Some(R12), Some(R0))
        irProgram.instructions += Instr(POP, Some(R8))
        irProgram.instructions += Instr(ADD, Some(R12), Some(R12), Some(Imm(4)))
        irProgram.instructions += Instr(STR, Some(R8), Some(R12))
        irProgram.instructions += Instr(POP, Some(R8))
        irProgram.instructions += Instr(SUB, Some(R12), Some(R12), Some(Imm(4)))
        irProgram.instructions += Instr(STR, Some(R8), Some(R12))
        irProgram.instructions += Instr(PUSH, Some(R12))
      }
      case default => throw new Exception("Invalid assignment, expected pair")
    }
  }

  /* Evaluates rvalue and places result on top of the stack */
  def buildRValue(rvalue: RValue, argType: SAType)(implicit
      irProgram: IRProgram
  ): Unit = {
    rvalue match {
      case e: Expression          => buildExpression(e)
      case ArrayLiteral(args)     => buildArrayLiteral(args, argType)
      case FunctionCall(id, args) => ???
      case NewPair(e1, e2)        => buildNewPair(e1, e2, argType)
      case PairElem(index, id)    => ???
      case default =>
        throw new Exception("Invalid rhs for assignment/declaration")
    }
  }

  def buildExit(expression: Expression)(implicit irProgram: IRProgram): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(BL, Some(LabelRef("exit")))
  }

  def buildAssignment(lvalue: LValue, rvalue: RValue)(implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    buildRValue(
      rvalue,
      irProgram.symbolTable.lookupType(lvalue match {
        case i @ Identifier(_) => i
        case default =>
          throw new Exception("Attempted type lookup of non-identifier")
      })
    )
    convertLValueToOperand(lvalue) match {
      case None => return
      case Some(x) => {
        irProgram.instructions += Instr(POP, Some(R8))
        irProgram.instructions += Instr(STR, Some(x), Some(R8))
      }
    }
  }

  def buildIf(
      condition: Expression,
      thenBody: List[Statement],
      elseBody: List[Statement]
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    val elseLabel = ".L" + irProgram.labelCount
    irProgram.labelCount += 1
    buildExpression(condition)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(elseLabel)),
      None,
      None,
      NE
    )
    enterScope()
    thenBody.foreach(buildStatement(_))
    exitScope()
    irProgram.instructions += Label(elseLabel)
    enterScope()
    elseBody.foreach(buildStatement(_))
    exitScope()
  }

  def buildWhile(condition: Expression, body: List[Statement])(implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    val conditionLabel = ".L" + irProgram.labelCount
    val doneLabel = ".L" + irProgram.labelCount + 1
    irProgram.labelCount += 2
    buildExpression(condition)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(doneLabel)),
      None,
      None,
      NE
    )
    enterScope()
    body.foreach(buildStatement(_))
    exitScope()
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(conditionLabel)),
      None,
      None
    )
    irProgram.instructions += Label(doneLabel)
  }

  def getExpressionType(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): SAType = {
    expression match {
      case IntLiteral(_)    => SAIntType
      case CharLiteral(_)   => SACharType
      case BoolLiteral(_)   => SABoolType
      case StringLiteral(_) => SAStringType
      case PairLiteral      => SAPairType(SAAnyType, SAAnyType)
      case Identifier(id) =>
        irProgram.symbolTable.lookupType(Identifier(id)((0, 0)))
      case ArrayElem(id, indices)   => SAIntType
      case BinaryOpApp(_, lexpr, _) => getExpressionType(lexpr)
      case UnaryOpApp(op, expr) =>
        op match {
          case Negation => SAIntType
          case Not      => SABoolType
          case Len      => SAIntType
          case Ord      => SAIntType
          case Chr      => SACharType
        }
      case default => throw new Exception("Invalid expression type")
    }
  }

  def buildPrint(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(PRINT(getExpressionType(expression)))
  }

  def buildFree(expression: Expression): Unit = {}

  def buildReturn(expression: Expression): Unit = {}

  def buildRead(lvalue: LValue): Unit = {}

  def enterScope()(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.symbolTable.enterScope()
    irProgram.instructions += EnterScope(
      irProgram.symbolTable.getScope()
    )
  }

  def exitScope()(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += ExitScope(
      irProgram.symbolTable.getScope()
    )
    irProgram.symbolTable.exitScope()
  }

  def buildBegin(
      statements: List[Statement]
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    enterScope()
    statements.foreach(buildStatement(_))
    exitScope()
  }

  def buildPrintln(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildPrint(expression)
    irProgram.instructions += Instr(PRINTLN)
  }

  def buildDeclaration(argType: SAType, id: Identifier, rvalue: RValue)(implicit
      irProgram: IRProgram,
      funcName: String
  ) = {
    buildAssignment(id, rvalue)
  }

  def buildStatement(
      statement: Statement
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    statement match {
      case ExitStatement(e) => buildExit(e)
      case AssignmentStatement(lvalue, rvalue) =>
        buildAssignment(lvalue, rvalue)
      case DeclarationStatement(argType, id, rvalue) =>
        buildDeclaration(convertSyntaxToTypeSys(argType), id, rvalue)
      case IfStatement(e, body1, body2) => buildIf(e, body1, body2)
      case SkipStatement                => return
      case WhileStatement(e, body)      => buildWhile(e, body)
      case PrintStatement(e)            => buildPrint(e)
      case FreeStatement(e)             => buildFree(e)
      case ReturnStatement(e)           => buildReturn(e)
      case BeginStatement(statements)   => buildBegin(statements)
      case ReadStatement(e)             => buildRead(e)
      case PrintLnStatement(e)          => buildPrintln(e)
      case _ => throw new Exception("Invalid statement type")
    }
  }

  def renameFunc(functionName: String) = "wacc_" + functionName

  def buildFunc(
      func: Func
  )(implicit irProgram: IRProgram, funcName: String): ListBuffer[IRType] = {
    irProgram.instructions += Label(
      renameFunc(func.identBinding.identifier.name)
    )
    func.body.foreach(buildStatement(_))
    irProgram.instructions
  }

  def buildFuncs(
      functions: List[Func]
  )(implicit irProgram: IRProgram): ListBuffer[IRType] = {
    functions.foreach((func: Func) =>
      buildFunc(func)(irProgram, func.identBinding.identifier.name)
    )
    irProgram.instructions
  }

  def buildIR(
      ast: Program,
      symbolTable: SymbolTable
  ): (ListBuffer[IRType], SymbolTable) = {
    implicit val irProgram = IRProgram(ListBuffer(), 0, 0, symbolTable)
    implicit val funcName = "0"
    buildFuncs(ast.functions)
    ast.statements.foreach(buildStatement(_))
    (irProgram.instructions, irProgram.symbolTable)
  }
}
