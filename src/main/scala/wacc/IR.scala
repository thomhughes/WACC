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

  def getPairDestAddress(lvalue: LValue)(implicit irProgram: IRProgram): Unit = {
    lvalue match {
      case i@Identifier(_) => {
        irProgram.instructions += Instr(MOV, Some(R9), Some(Var(i.name)))
        irProgram.instructions += Instr(LDR, Some(R9), Some(AddrReg(R9, 0)))
        irProgram.instructions += Instr(PUSH, Some(R9))
      }
      case PairElem(index2,lvalue2) => {
        getPairDestAddress(lvalue2)
        irProgram.instructions += Instr(POP, Some(R9))
        index2 match {
          case Fst => {
            irProgram.instructions += Instr(LDR, Some(R9), Some(AddrReg(R9, 0)))
            irProgram.instructions += Instr(PUSH, Some(R9))
          }
          case Snd => {
            irProgram.instructions += Instr(LDR, Some(R9), Some(AddrReg(R9, 4)))
            irProgram.instructions += Instr(PUSH, Some(R9))
          }
        }
      }
      case ArrayElem(id,indices) => buildArrayAccess(id,indices)
    }
  }

  /* Updates lvalue with value currently at top of stack */
  def buildLValue(
      lvalue: LValue
  )(implicit irProgram: IRProgram): Unit = {
    lvalue match {
      case Identifier(x)           => {
        irProgram.instructions += Instr(POP, Some(R8))
        irProgram.instructions += Instr(STR, Some(R8), Some(Var(x)))
      }
      case ArrayElem(id, indices)  => {
        buildArrayReassignment(id, indices)
        return
      }
      case PairElem(index1, lvalue1) => {
        getPairDestAddress(lvalue1)
        irProgram.instructions += Instr(POP, Some(R9))
        irProgram.instructions += Instr(POP, Some(R8))
        index1 match {
          case Fst => irProgram.instructions += Instr(STR, Some(R8), Some(AddrReg(R9, 0)))
          case Snd => irProgram.instructions += Instr(STR, Some(R8), Some(AddrReg(R9, 4)))
        }
      }
      case _ => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

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
      case Negation => Instr(RSB, Some(R8), Some(R8), Some(Imm(0)))
      case Len      => Instr(LEN, Some(R8))
    })
    irProgram.instructions += Instr(PUSH, Some(R8))
  }

  /* Evaluates expression and places result on top of the stack */
  def buildExpression(
      expr: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    implicit def boolToByte(bool: Boolean): Byte = if (bool) 1 else 0
    expr match {
      case IntLiteral(value) => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(value)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case CharLiteral(char) => {
        irProgram.instructions += Instr(MOVB, Some(R8), Some(ImmB(char.toByte)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case BoolLiteral(bool) => {
        irProgram.instructions += Instr(MOVB, Some(R8), Some(ImmB(bool)))
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
      case id @ Identifier(_) => {
        val loadDataOperand = irProgram.symbolTable.lookupType(id) match {
          case SABoolType | SACharType => LDRB
          case _                       => LDR
        }
        irProgram.instructions += Instr(
          loadDataOperand,
          convertLValueToOperand(id),
          Some(R8)
        )
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
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    /*
    we need to return something that can then be stored in a given variable
    the variable will store the address that has been malloced which will be
    at the top of the stack
    length offset is used to store the length of each array before each elem
     */
    val bytesPerInt = 4
    val lengthOffset = 1 * bytesPerInt
    val elementSize = getNoBytes(argType)
    irProgram.instructions += Instr(
      MOV,
      Some(R0),
      Some(Imm(args.length * elementSize + lengthOffset))
    )
    irProgram.instructions += Instr(MALLOC)
    irProgram.instructions += Instr(MOV, Some(R12), Some(R0))
    irProgram.instructions += Instr(
      ADD,
      Some(R12),
      Some(R12),
      Some(Imm(lengthOffset))
    )
    irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(args.length)))
    irProgram.instructions += Instr(
      STR,
      Some(R8),
      Some(AddrReg(R12, -lengthOffset))
    )
    for (i <- 0 until args.length) {
      buildExpression(args(i))
      irProgram.instructions += Instr(POP, Some(R8))
      irProgram.instructions += Instr(
        STR,
        Some(R8),
        Some(AddrReg(R12, i * elementSize))
      )
    }
    // irProgram.instructions += Instr(STR, Some(R12), Some(ArrayToStore(args)))
    irProgram.instructions += Instr(PUSH, Some(R12))
  }

  def buildFuncCall(id: Identifier, args: List[Expression])(implicit
      irProgram: IRProgram,
      funcName: String
  ) = {
    // build expressions in order, will be reversed on stack
    args.foreach(buildExpression(_))
    irProgram.instructions += Instr(BL, Some(LabelRef(id.name)))
    irProgram.instructions += Instr(PUSH, Some(R0))
  }

  // When this is called, arg to be stored is in R8
  def buildArrayReassignment(id: Identifier, indices: List[Expression])(implicit
      irProgram: IRProgram,
      funcName: String
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
      irProgram: IRProgram,
      funcName: String
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
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    irProgram.instructions += Instr(LDR, Some(R9), Some(Var(id.name)))
    irProgram.instructions += Instr(PUSH, Some(R9))
    def buildArrLoadHelper(indices: List[Expression]): Unit =
      (indices: @unchecked) match {
        case head :: Nil =>
          buildExpression(
            head
          ) // this will put the index on the top of the stack
        case head :: next => {
          // Load the current array pointer into R3
          irProgram.instructions += Instr(POP, Some(R3))
          buildExpression(head)
          // arg is now on top of stack: we will pop and proceed
          irProgram.instructions += Instr(POP, Some(R10))
          irProgram.instructions += Instr(ARRLOAD)
          irProgram.instructions += Instr(PUSH, Some(R3))
          buildArrLoadHelper(next)
        }
      }
    buildArrLoadHelper(indices)
  }

  def buildPairElement(element: Expression, noBytes: Int)(implicit
      irProgram: IRProgram,
      funcName: String
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
      irProgram: IRProgram,
      funcName: String
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
        irProgram.instructions += Instr(STR, Some(R8), Some(AddrReg(R12, 4)))
        irProgram.instructions += Instr(POP, Some(R8))
        irProgram.instructions += Instr(SUB, Some(R12), Some(R12), Some(Imm(4)))
        irProgram.instructions += Instr(STR, Some(R8), Some(R12))
        irProgram.instructions += Instr(PUSH, Some(R12))
      }
      case default => throw new Exception("Invalid assignment, expected pair")
    }
  }

  def buildPairElem(index: PairIndex)(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += Instr(LDR, Some(R8), Some(AddrReg(R8, 0)))
    index match {
      case Fst => {
        irProgram.instructions += Instr(LDR, Some(R8), Some(AddrReg(R8, 0)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case Snd => {
        irProgram.instructions += Instr(LDR, Some(R8), Some(AddrReg(R8, 4)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
    }
  }

  /* Evaluates rvalue and places result on top of the stack */
  def buildRValue(rvalue: RValue, argType: SAType)(implicit
<<<<<<< HEAD
      irProgram: IRProgram,
      funcName: String
=======
      irProgram: IRProgram, funcName: String
>>>>>>> 6a8b2660bc87f4fc1ac22539b5bd19c654d18e3e
  ): Unit = {
    rvalue match {
      case e: Expression          => buildExpression(e)
      case ArrayLiteral(args)     => buildArrayLiteral(args, argType)
      case FunctionCall(id, args) => buildFuncCall(id, args)
      case NewPair(e1, e2)        => buildNewPair(e1, e2, argType)
      case PairElem(index1, id1)    => id1 match {
        case Identifier(id) => {
          irProgram.instructions += Instr(MOV, Some(R8), Some(Var(id)))
          buildPairElem(index1)
        }
        case p@PairElem(index2, id2) => {
          buildRValue(p, SAPairRefType)
          irProgram.instructions += Instr(POP, Some(R8))
          buildPairElem(index2)
        }
        // Whether multi-dimensional or not the elem must be a memory location of a pair
        case ArrayElem(id,indices) => {
          buildArrayAccess(id,indices)
          irProgram.instructions += Instr(POP, Some(R8))
        }
        case default => throw new Exception("Invalid pair elem")
      }
      case default =>
        throw new Exception("Invalid rhs for assignment/declaration")
    }
  }

  def buildExit(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
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
        case ArrayElem(id, _)  => id
        case default =>
          throw new Exception("Attempted type lookup of non-identifier")
      })
    )
    buildLValue(lvalue)
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
    irProgram.symbolTable.enterScope()
    thenBody.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
    irProgram.instructions += Label(elseLabel)
    irProgram.symbolTable.enterScope()
    elseBody.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
  }

  def buildWhile(condition: Expression, body: List[Statement])(implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    val conditionLabel = ".L" + irProgram.labelCount
    val doneLabel = ".L" + irProgram.labelCount + 1
    irProgram.labelCount += 2
    irProgram.instructions += Label(conditionLabel)
    buildExpression(condition)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(doneLabel)),
      None,
      None,
      NE
    )
    irProgram.symbolTable.enterScope()
    body.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
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

  def buildFree(expression: Expression)(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(FREE(getExpressionType(expression)))
  }

  def buildReturn(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    buildFuncEpilogue()
  }

  def buildRead(lvalue: LValue): Unit = {}

  def buildBegin(
      statements: List[Statement]
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    irProgram.symbolTable.enterScope()
    statements.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
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

  def buildFuncPrologue()(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += Instr(PUSH, Some(LR))
    irProgram.instructions += Instr(PUSH, Some(FP))
    // Store frame pointer as pointer to frame pointer on stack
    irProgram.instructions += Instr(ADD, Some(FP), Some(SP), Some(Imm(4)))
    val frameSize = irProgram.symbolTable.getFrameSize()
    if (irProgram.symbolTable.getFrameSize() > 0) {
      irProgram.instructions += Instr(
        SUB,
        Some(SP),
        Some(SP),
        Some(Imm(frameSize))
      )
    }
  }

  // Inserted after function returns (all functions must return to be correct)
  def buildFuncEpilogue()(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += Instr(SUB, Some(SP), Some(FP), Some(Imm(4)))
    irProgram.instructions += Instr(POP, Some(FP))
    irProgram.instructions += Instr(POP, Some(PC))
    irProgram.instructions += Instr(LTORG)
  }

  def buildFunc(
      func: Func
  )(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += Label(
      renameFunc(func.identBinding.identifier.name)
    )
    irProgram.symbolTable.resetScope()
    irProgram.symbolTable.enterScope()
    buildFuncPrologue()
    func.body.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
  }

  def buildFuncs(
      functions: List[Func]
  )(implicit irProgram: IRProgram) = {
    functions.foreach((func: Func) =>
      buildFunc(func)(irProgram, func.identBinding.identifier.name)
    )
  }

  def buildMain(statements: List[Statement])(implicit
      irProgram: IRProgram
  ) = {
    implicit val funcName = "0"
    irProgram.symbolTable.resetScope()
    irProgram.symbolTable.enterScope()
    buildFuncPrologue()
    statements.foreach(buildStatement(_))
    buildFuncEpilogue()
    irProgram.symbolTable.exitScope()
  }

  def buildIR(
      ast: Program,
      symbolTable: SymbolTable
  ): (ListBuffer[IRType], SymbolTable) = {
    implicit val irProgram = IRProgram(ListBuffer(), 0, 0, symbolTable)
    buildFuncs(ast.functions)
    buildMain(ast.statements)
    (irProgram.instructions, irProgram.symbolTable)
  }
}
