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
    case SAIntType | SAArrayType(_, _) | SAPairType(_, _) | SAStringType |
        SAPairRefType | SAUnknownType =>
      4
    case SABoolType | SACharType => 1
    case unexpected =>
      throw new Exception("Unexpected LValue type: " + unexpected)
  }

  def getLoadDataOperand(t: SAType): MemAccess =
    if (getNoBytes(t) == 1) LDRB else LDR

  def getStoreDataOperand(t: SAType): MemAccess =
    if (getNoBytes(t) == 1) STRB else STR

  def constantToChunks(value: Int): List[Int] = {
    val absVal = Math.abs(value)
    List(
      absVal & 0xff,
      absVal & (0xff << 8),
      absVal & (0xff << 16),
      absVal & (0xff << 24)
    ).filter(_ != 0)
  }

  def loadRemainingChunks(positive: Boolean, dest: Register, chunks: List[Int])(
      implicit irProgram: IRProgram
  ) = {
    val opcode = if (positive) ADD else SUB
    chunks.foreach(x =>
      irProgram.instructions += Instr(
        opcode,
        Some(dest),
        Some(dest),
        Some(Imm(x))
      )
    )
  }

  def loadMovConstant(dest: Register, value: Int)(implicit
      irProgram: IRProgram
  ) = {
    val chunks = constantToChunks(value)
    irProgram.instructions += Instr(MOV, Some(dest), Some(Imm(0)))
    loadRemainingChunks(value > 0, dest, chunks)
  }

  // If src is defined then it's offset from source, if not then it's offset from 0
  def loadRegConstant(dest: Register, src: Register, value: Int)(implicit
      irProgram: IRProgram
  ) = {
    val chunks = constantToChunks(value)
    val opcode = if (value > 0) ADD else SUB
    chunks match {
      case Nil =>
        irProgram.instructions += Instr(MOV, Some(dest), Some(Imm(0)))
      case head :: next => {
        irProgram.instructions += Instr(
          opcode,
          Some(dest),
          Some(src),
          Some(Imm(head))
        )
        loadRemainingChunks(value > 0, dest, next)
      }
    }
  }

  def getScope()(implicit irProgram: IRProgram, funcName: String) =
    irProgram.symbolTable.getScope()

  /* Pushes lvalue reference to top of stack */
  def buildLValueReference(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    lvalue match {
      case id @ Identifier(_) => {
        loadRegConstant(R0, FP, irProgram.symbolTable.lookupAddress(id))
        irProgram.instructions += Instr(PUSH, Some(R0))
      }
      case ArrayElem(id, indices) => buildArrayLoadReference(id, indices)
      case PairElem(index, innerLValue) => {
        buildLValueReference(innerLValue)
        buildStackDereference(getLValueType(innerLValue))
        irProgram.instructions += Instr(POP, Some(R0))
        index match {
          case Fst =>
            irProgram.instructions += Instr(BL, Some(LabelRef("pair_fst")))
          case Snd =>
            irProgram.instructions += Instr(BL, Some(LabelRef("pair_snd")))
        }
        irProgram.instructions += Instr(PUSH, Some(R0))
      }
      case _ => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

  // dereference value at top of stack *stack, clobbers R0
  def buildStackDereference(t: SAType)(implicit irProgram: IRProgram): Unit = {
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(
      getLoadDataOperand(t),
      Some(R0),
      Some(AddrReg(R0, 0))
    )
    irProgram.instructions += Instr(PUSH, Some(R0))
  }

  private def getPairElemType(index: PairIndex, pair: LValue)(implicit
      irProgram: IRProgram,
      funcName: String
  ): SAType =
    getLValueType(pair) match {
      case SAPairType(fstType, sndType) =>
        index match {
          case Fst => fstType
          case Snd => sndType
        }
      case SAPairRefType => SAUnknownType
      case t => throw new Exception(s"Invalid type $t for pair element")
    }

  private def getArrayElemType(id: Identifier, indices: List[Expression])(
      implicit
      irProgram: IRProgram,
      funcName: String
  ): SAType =
    irProgram.symbolTable.lookupType(id) match {
      case SAArrayType(arrayType, arity) => {
        if (indices.length < arity)
          SAArrayType(arrayType, arity - indices.length)
        else if (indices.length == arity)
          arrayType
        else
          throw new Exception(
            "Too many indices for array ${id.name} of arity $arity"
          )
      }
      case _ => throw new Exception("Invalid type for array element")
    }

  def getLValueType(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): SAType =
    lvalue match {
      case id @ Identifier(_) => irProgram.symbolTable.lookupType(id)
      case PairElem(anotherIndex, anotherPair) =>
        getPairElemType(anotherIndex, anotherPair)
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case _                      => throw new Exception("Exhaustive")
    }

  /* Stores top of stack into lvalue */
  def buildLValue(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildLValueReference(lvalue)
    irProgram.instructions += Instr(POP, Some(R1))
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(
      getStoreDataOperand(getLValueType(lvalue)),
      Some(R0),
      Some(AddrReg(R1, 0))
    )
  }

  def nonCompareInstruction(op: BinaryOp)(implicit irProgram: IRProgram): Unit =
    (op: @unchecked) match {
      case Plus => {
        irProgram.instructions += Instr(ADDS, Some(R8), Some(R8), Some(R9))
        irProgram.instructions += Instr(
          BL,
          Some(LabelRef("error_arithmetic_overflow")),
          VS
        )
      }
      case Minus => {
        irProgram.instructions += Instr(SUBS, Some(R8), Some(R8), Some(R9))
        irProgram.instructions += Instr(
          BL,
          Some(LabelRef("error_arithmetic_overflow")),
          VS
        )
      }
      case Mul => {
        irProgram.instructions += Instr(
          SMULL,
          Some(JoinedRegister(R8, R9)),
          Some(JoinedRegister(R8, R9))
        )
        irProgram.instructions += Instr(
          CMP,
          Some(R9),
          Some(ShiftedRegister(R8, Shift(ASR, 31)))
        )
        irProgram.instructions += Instr(
          BL,
          Some(LabelRef("error_arithmetic_overflow")),
          NE
        )
      }
      case Div => {
        irProgram.instructions += Instr(DIV, Some(R8), Some(R8), Some(R9))
      }
      case Mod => {
        irProgram.instructions += Instr(MOD, Some(R8), Some(R8), Some(R9))
      }
    }

  def logicalCompareInstruction(op: BinaryOp)(implicit irProgram: IRProgram) = {
    val doneLabel = s".L${irProgram.labelCount}"
    irProgram.labelCount += 1
    irProgram.instructions += Instr(CMP, Some(R8), Some(Imm(0)))
    (op: @unchecked) match {
      case And => {
        irProgram.instructions += Instr(
          MOV,
          Some(R8),
          Some(Imm(0)),
          EQ
        )
        irProgram.instructions += Instr(
          B,
          Some(LabelRef(doneLabel)),
          EQ
        )
      }
      case Or => {
        irProgram.instructions += Instr(
          MOV,
          Some(R8),
          Some(Imm(1)),
          NE
        )
        irProgram.instructions += Instr(
          B,
          Some(LabelRef(doneLabel)),
          NE
        )
      }
    }
    irProgram.instructions += Instr(CMP, Some(R9), Some(Imm(0)))
    irProgram.instructions += Instr(
      MOV,
      Some(R8),
      Some(Imm(1)),
      NE
    )
    irProgram.instructions += Instr(
      MOV,
      Some(R8),
      Some(Imm(0)),
      EQ
    )
    irProgram.instructions += Label(doneLabel)
  }

  def compareInstruction(op: BinaryOp)(implicit irProgram: IRProgram) = {
    irProgram.instructions += Instr(CMP, Some(R8), Some(R9))
    irProgram.instructions += Instr(
      MOV,
      Some(R8),
      Some(Imm(1)),
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
      (op: @unchecked) match {
        case Eq  => NE
        case Gt  => LE
        case Ge  => LT
        case Lt  => GE
        case Le  => GT
        case Neq => EQ
      }
    )
  }

  def modifyingUnaryOp(op: UnaryOp)(implicit irProgram: IRProgram): Unit = {
    irProgram.instructions += Instr(POP, Some(R0))
    (op: @unchecked) match {
      case Not => {
        irProgram.instructions += Instr(CMP, Some(R0), Some(Imm(0)))
        irProgram.instructions += Instr(
          MOV,
          Some(R0),
          Some(Imm(0)),
          NE
        )
        irProgram.instructions += Instr(
          MOV,
          Some(R0),
          Some(Imm(1)),
          EQ
        )
      }
      case Negation => {
        irProgram.instructions += Instr(RSBS, Some(R0), Some(R0), Some(Imm(0)))
        irProgram.instructions += Instr(
          BL,
          Some(LabelRef("error_arithmetic_overflow")),
          VS
        )
      }
      case Len =>
        irProgram.instructions += Instr(BL, Some(LabelRef("array_size")))
    }
    irProgram.instructions += Instr(PUSH, Some(R0))
  }

  /* Evaluates expression and places result on top of the stack */
  def buildExpression(
      expr: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    implicit def boolToInt(bool: Boolean): Int = if (bool) 1 else 0
    expr match {
      case IntLiteral(value) => {
        loadMovConstant(R8, value)
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case CharLiteral(char) => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(char.toInt)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case BoolLiteral(bool) => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(bool.toInt)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case StringLiteral(string) => {
        val label = LabelRef(".L.str" + irProgram.stringLiteralCounter)
        irProgram.instructions += Data(label, string)
        irProgram.stringLiteralCounter += 1
        irProgram.instructions += Instr(LDR, Some(R8), Some(label))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case PairLiteral => {
        irProgram.instructions += Instr(MOV, Some(R8), Some(Imm(0)))
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case id @ Identifier(_) => {
        irProgram.instructions += Instr(
          getLoadDataOperand(irProgram.symbolTable.lookupType(id)),
          Some(R8),
          Some(AddrReg(FP, irProgram.symbolTable.lookupAddress(id)))
        )
        irProgram.instructions += Instr(PUSH, Some(R8))
      }
      case BinaryOpApp(op, lexpr, rexpr) => {
        buildExpression(lexpr)
        buildExpression(rexpr)
        irProgram.instructions += Instr(POP, Some(R9))
        irProgram.instructions += Instr(POP, Some(R8))
        op match {
          case Plus | Minus | Mul | Div | Mod => nonCompareInstruction(op)
          case And | Or                       => logicalCompareInstruction(op)
          case default                        => compareInstruction(op)
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
    val elementSize = argType match {
      case SAArrayType(arrayType, 1) => getNoBytes(arrayType)
      case SAArrayType(_, _)         => 4
      case _ => throw new Exception("Unexpected LValue type")
    }
    irProgram.instructions += Instr(MOV, Some(R0), Some(Imm(elementSize)))
    irProgram.instructions += Instr(MOV, Some(R1), Some(Imm(args.size)))
    args.reverse.foreach(buildExpression(_))
    irProgram.instructions += Instr(BL, Some(LabelRef("array_literal_create")))
    irProgram.instructions += Instr(PUSH, Some(R0))
  }

  def buildFuncCall(id: Identifier, args: List[Expression])(implicit
      irProgram: IRProgram,
      funcName: String
  ) = {
    // build expressions in order, will be reversed on stack
    args.reverse.foreach(buildExpression(_))
    irProgram.instructions += Instr(BL, Some(LabelRef(renameFunc(id.name))))
    irProgram.instructions += Instr(PUSH, Some(R0))
  }

  def buildArrayAccess(id: Identifier, indices: List[Expression])(implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    buildArrayLoadReference(id, indices)
    buildStackDereference(getArrayElemType(id, indices))
  }

  // after this function is called, the pointer to the list will be on the top of the stack
  def buildArrayLoadReference(id: Identifier, indices: List[Expression])(
      implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    buildLValueReference(id)
    def buildArrLoadHelper(indices: List[Expression]): Unit = indices match {
      case Nil => {}
      case head :: next => {
        buildExpression(head)
        // arg is now on top of stack: we will pop and proceed
        irProgram.instructions += Instr(POP, Some(R1))
        buildStackDereference(getArrayElemType(id, next))
        irProgram.instructions += Instr(POP, Some(R0))
        irProgram.instructions += Instr(BL, Some(LabelRef("array_access")))
        irProgram.instructions += Instr(PUSH, Some(R0))
        buildArrLoadHelper(next)
      }
    }
    buildArrLoadHelper(indices)
  }

  def buildNewPair(e1: Expression, e2: Expression, argType: SAType)(implicit
      irProgram: IRProgram,
      funcName: String
  ) = {
    buildExpression(e2)
    buildExpression(e1)
    irProgram.instructions += Instr(BL, Option(LabelRef("pair_create")))
    irProgram.instructions += Instr(PUSH, Some(R0))
  }

  /* Evaluates rvalue and places result on top of the stack */
  def buildRValue(rvalue: RValue, argType: SAType)(implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    rvalue match {
      case e: Expression          => buildExpression(e)
      case ArrayLiteral(args)     => buildArrayLiteral(args, argType)
      case FunctionCall(id, args) => buildFuncCall(id, args)
      case NewPair(e1, e2)        => buildNewPair(e1, e2, argType)
      case p @ PairElem(index, innerLValue) => {
        buildLValueReference(p)
        buildStackDereference(getPairElemType(index, innerLValue))
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
      getLValueType(lvalue)
    )
    buildLValue(lvalue)
  }

  def buildIf(
      condition: Expression,
      thenBody: List[Statement],
      elseBody: List[Statement]
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    val elseLabel = s".L${irProgram.labelCount}"
    val ifEndLabel = s".L${irProgram.labelCount + 1}"
    irProgram.labelCount += 2
    buildExpression(condition)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(CMP, Some(R8), Some(Imm(1)))
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(elseLabel)),
      NE
    )
    irProgram.symbolTable.enterScope()
    thenBody.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
    irProgram.instructions += Instr(B, Option(LabelRef(ifEndLabel)))
    irProgram.instructions += Label(elseLabel)
    irProgram.symbolTable.enterScope()
    elseBody.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
    irProgram.instructions += Label(ifEndLabel)
  }

  def buildWhile(condition: Expression, body: List[Statement])(implicit
      irProgram: IRProgram,
      funcName: String
  ): Unit = {
    val conditionLabel = s".L${irProgram.labelCount}"
    val doneLabel = s".L${irProgram.labelCount + 1}"
    irProgram.labelCount += 2
    irProgram.instructions += Label(conditionLabel)
    buildExpression(condition)
    irProgram.instructions += Instr(POP, Some(R8))
    irProgram.instructions += Instr(CMP, Some(R8), Some(Imm(1)))
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(doneLabel)),
      NE
    )
    irProgram.symbolTable.enterScope()
    body.foreach(buildStatement(_))
    irProgram.symbolTable.exitScope()
    irProgram.instructions += Instr(
      B,
      Option(LabelRef(conditionLabel))
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
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case BinaryOpApp(op, lexpr, _) =>
        op match {
          case Eq | Gt | Ge | Lt | Le | Neq | And | Or => SABoolType
          case _ => getExpressionType(lexpr)
        }
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
    val exprType = getExpressionType(expression)

    // Dereference arrays to access value of data
    exprType match {
      case at @ SAArrayType(_, _) => buildStackDereference(at)
      case _                      => ()
    }
    irProgram.instructions += Instr(POP, Some(R0))
    irProgram.instructions += Instr(PRINT(exprType))
  }

  def buildFree(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    getExpressionType(expression) match {
      case SAArrayType(_, _) =>
        irProgram.instructions += Instr(BL, Some(LabelRef("array_free")))
      case SAPairType(_, _) =>
        irProgram.instructions += Instr(BL, Some(LabelRef("pair_free")))
      case unexpected =>
        throw new Exception("Unable to free type: " + unexpected)
    }
  }

  def buildReturn(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, Some(R0))
    buildFuncEpilogue()
  }

  def buildRead(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildLValueReference(lvalue)
    irProgram.instructions += Instr(POP, Some(R0))
    val noBytes = getNoBytes(getLValueType(lvalue))
    loadMovConstant(R1, noBytes);
    irProgram.instructions += Instr(BL, Some(LabelRef("read")))
  }

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
    irProgram.symbolTable.encountered(id)
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
      loadRegConstant(SP, SP, -frameSize)
    }
  }

  // Inserted after function returns (all functions must return to be correct)
  def buildFuncEpilogue()(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += Instr(SUB, Some(SP), Some(FP), Some(Imm(4)))
    irProgram.instructions += Instr(POP, Some(FP))
    irProgram.instructions += Instr(POP, Some(PC))
    irProgram.instructions += Ltorg
  }

  def buildFunc(
      func: Func
  )(implicit irProgram: IRProgram, funcName: String) = {
    irProgram.instructions += Label(
      renameFunc(func.identBinding.identifier.name)
    )
    irProgram.symbolTable.resetScope()
    func.params.foreach((param: Parameter) =>
      irProgram.symbolTable.encountered(param.identifier)
    )
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
    irProgram.instructions += Global("main")
    irProgram.instructions += Label("main")
    irProgram.symbolTable.resetScope()
    irProgram.symbolTable.enterScope()
    buildFuncPrologue()
    statements.foreach(buildStatement(_))
    irProgram.instructions += Instr(MOV, Some(R0), Some(Imm(0)))
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
