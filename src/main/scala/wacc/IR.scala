package wacc

case class IsLastStatement(last: Boolean)
case class IsFunctionInlined(inlined: Boolean)

object IR {
  import scala.collection.mutable.ListBuffer
  import scala.language.implicitConversions
  import scala.collection.mutable.Map

  import wacc.SymbolTable

  // map from scope no to no of bytes
  import AST._
  import Types._
  import Analyser.convertSyntaxToTypeSys

  val lengthOfSmallFunction = 6
  val maxNoCallsOfFunction = 6

  def countFunctionCalls(statement: Statement, function: Func): Int = {
    statement match {
      case AssignmentStatement(_, rvalue) =>
        rvalue match {
          case FunctionCall(id, _) =>
            if (id.name == function.identBinding.identifier.name) 1 else 0
          case default => 0
        }
      case DeclarationStatement(_, _, rvalue) =>
        rvalue match {
          case FunctionCall(id, _) =>
            if (id.name == function.identBinding.identifier.name) 1 else 0
          case default => 0
        }
      case IfStatement(_, thenBody, elseBody) => {
        thenBody.map(countFunctionCalls(_, function)).sum + elseBody
          .map(countFunctionCalls(_, function))
          .sum
      }
      case WhileStatement(_, body) => {
        body.map(countFunctionCalls(_, function)).sum
      }
      case BeginStatement(body) => {
        body.map(countFunctionCalls(_, function)).sum
      }
      case default => 0
    }
  }
  private val paramRegs = List(R0, R1, R2, R3, R4)
  private val scratchRegs = List(R8, R9)

  def getNoFunctionCalls(program: Program, function: Func): Int = {
    program.statements.map(countFunctionCalls(_, function)).sum
  }

  def functionNotNested(statement: Statement, func: Func): Boolean = {
    statement match {
      case AssignmentStatement(_, rvalue) =>
        rvalue match {
          case FunctionCall(_, _) => false
          case default            => true
        }
      case DeclarationStatement(_, _, rvalue) =>
        rvalue match {
          case FunctionCall(_, _) => false
          case default            => true
        }
      case IfStatement(_, thenBody, elseBody) => {
        thenBody.forall(functionNotNested(_, func)) && elseBody.forall(
          functionNotNested(_, func))
      }
      case WhileStatement(_, body) => {
        body.forall(functionNotNested(_, func))
      }
      case BeginStatement(body) => {
        body.forall(functionNotNested(_, func))
      }
      case default => true
    }
  }

  def shouldInlineFunction(program: Program, function: Func): Boolean = {
    (function.body.length < lengthOfSmallFunction || getNoFunctionCalls(
      program,
      function) < maxNoCallsOfFunction) && (
      (
        function.body.forall(
          functionNotNested(_, function)
        ))
    )
  }

  def getInlinedFunctions(program: Program)
    : Map[String, (Int, List[Parameter], List[Statement])] = {
    val inlinedFunctionsAndBodies
      : Map[String, (Int, List[Parameter], List[Statement])] = Map()
    program.functions.foreach(
      func => {
        if (shouldInlineFunction(program, func)) {
          inlinedFunctionsAndBodies += ((func.identBinding.identifier.name,
                                         (0, func.params, func.body)))
        }
      }
    )
    inlinedFunctionsAndBodies
  }

  def changeScope(enter: Boolean)(implicit irProgram: IRProgram,
                                  funcName: String) = {
    if (enter) irProgram.symbolTable.enterScope()
    else irProgram.symbolTable.exitScope()
    irProgram.instructions += Instr(PUSH, RegisterList(List(R0)))
    loadMovConstant(paramRegs(0), irProgram.symbolTable.getScope())
    val changeFunction =
      if (enter) BranchLabel("enter_scope") else BranchLabel("exit_scope")
    irProgram.instructions += Instr(BL, changeFunction)
    irProgram.instructions += Instr(POP, RegisterList(List(R0)))
  }

  private def getLoadDataOperand(t: SAType): MemAccess =
    if (getNoBytes(t) == 1) LDRB else LDR

  private def getStoreDataOperand(t: SAType): MemAccess =
    if (getNoBytes(t) == 1) STRB else STR

  private def constantToChunks(value: Int): List[Int] = {
    val absVal = Math.abs(value)
    List(
      absVal & 0xff,
      absVal & (0xff << 8),
      absVal & (0xff << 16),
      absVal & (0xff << 24)
    ).filter(_ != 0)
  }

  private def loadRemainingChunks(positive: Boolean,
                                  dest: Register,
                                  chunks: List[Int])(
      implicit irProgram: IRProgram
  ) = {
    val opcode = if (positive) ADD else SUB
    chunks.foreach(
      x =>
        irProgram.instructions += Instr(
          opcode,
          dest,
          dest,
          Imm(x)
      ))
  }

  private def loadMovConstant(dest: Register, value: Int)(
      implicit
      irProgram: IRProgram) = {
    val chunks = constantToChunks(value)
    irProgram.instructions += Instr(MOV, dest, Imm(0))
    loadRemainingChunks(value > 0, dest, chunks)
  }

  // If src is defined then it's offset from source, if not then it's offset from 0
  private def loadRegConstant(dest: Register, src: Register, value: Int)(
      implicit
      irProgram: IRProgram) = {
    val chunks = constantToChunks(value)
    val opcode = if (value > 0) ADD else SUB
    chunks match {
      case Nil =>
        irProgram.instructions += Instr(MOV, dest, Imm(0))
      case head :: next => {
        irProgram.instructions += Instr(
          opcode,
          dest,
          src,
          Imm(head)
        )
        loadRemainingChunks(value > 0, dest, next)
      }
    }
  }

  /* Pushes lvalue reference to top of stack */
  private def buildLValueReference(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    lvalue match {
      case id @ Identifier(_) => {
        loadRegConstant(paramRegs(0),
                        FP,
                        irProgram.symbolTable.lookupAddress(id))
        irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
      }
      case ArrayElem(id, indices) => buildArrayLoadReference(id, indices)
      case PairElem(index, innerLValue) => {
        buildLValueReference(innerLValue)
        buildStackDereference(getLValueType(innerLValue))
        irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
        index match {
          case Fst =>
            irProgram.instructions += Instr(BL, BranchLabel("pair_fst"))
          case Snd =>
            irProgram.instructions += Instr(BL, BranchLabel("pair_snd"))
        }
        irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
      }
      case _ => throw new Exception("Invalid lhs for assignment/declaration")
    }
  }

  // dereference value at top of stack *stack, clobbers paramRegs(0)
  def buildStackDereference(t: SAType)(implicit irProgram: IRProgram): Unit = {
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    irProgram.instructions += Instr(
      getLoadDataOperand(t),
      paramRegs(0),
      AddrReg(paramRegs(0), 0)
    )
    irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
  }

  private def getPairElemType(index: PairIndex, pair: LValue)(
      implicit
      irProgram: IRProgram,
      funcName: String): SAType =
    getLValueType(pair) match {
      case SAPairType(fstType, sndType) =>
        index match {
          case Fst => fstType
          case Snd => sndType
        }
      case SAPairRefType => SAUnknownType
      case t             => throw new Exception(s"Invalid type $t for pair element")
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

  private def getLValueType(
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
  private def buildLValue(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildLValueReference(lvalue)
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(1))))
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    irProgram.instructions += Instr(
      getStoreDataOperand(getLValueType(lvalue)),
      paramRegs(0),
      AddrReg(paramRegs(1), 0)
    )
  }

  private def nonCompareInstruction(op: BinaryOp)(
      implicit irProgram: IRProgram): Unit =
    (op: @unchecked) match {
      case Plus => {
        irProgram.instructions += Instr(ADDS,
                                        scratchRegs(0),
                                        scratchRegs(0),
                                        scratchRegs(1))
        irProgram.instructions += Instr(
          BL,
          BranchLabel("error_arithmetic_overflow"),
          VS
        )
      }
      case Minus => {
        irProgram.instructions += Instr(SUBS,
                                        scratchRegs(0),
                                        scratchRegs(0),
                                        scratchRegs(1))
        irProgram.instructions += Instr(
          BL,
          BranchLabel("error_arithmetic_overflow"),
          VS
        )
      }
      case Mul => {
        irProgram.instructions += Instr(
          SMULL,
          JoinedRegister(scratchRegs(0), scratchRegs(1)),
          JoinedRegister(scratchRegs(0), scratchRegs(1))
        )
        irProgram.instructions += Instr(
          CMP,
          scratchRegs(1),
          ShiftedRegister(scratchRegs(0), Shift(ASR, 31))
        )
        irProgram.instructions += Instr(
          BL,
          BranchLabel("error_arithmetic_overflow"),
          NE
        )
      }
      case Div => {
        irProgram.instructions += Instr(MOV, paramRegs(0), scratchRegs(0))
        irProgram.instructions += Instr(MOV, paramRegs(1), scratchRegs(1))
        irProgram.instructions += Instr(DIV)
        irProgram.instructions += Instr(MOV, scratchRegs(0), paramRegs(0))
      }
      case Mod => {
        irProgram.instructions += Instr(MOV, paramRegs(0), scratchRegs(0))
        irProgram.instructions += Instr(MOV, paramRegs(1), scratchRegs(1))
        irProgram.instructions += Instr(MOD)
        irProgram.instructions += Instr(MOV, scratchRegs(0), paramRegs(1))
      }
    }

  private def getTemporaryLabelName(labelCount: Int): String =
    s".L${labelCount}"

  private def logicalCompareInstruction(op: BinaryOp)(
      implicit irProgram: IRProgram) = {
    val doneLabel = getTemporaryLabelName(irProgram.labelCount)
    irProgram.labelCount += 1
    irProgram.instructions += Instr(CMP, scratchRegs(0), Imm(0))
    (op: @unchecked) match {
      case And => {
        irProgram.instructions += Instr(
          MOV,
          scratchRegs(0),
          Imm(0),
          EQ
        )
        irProgram.instructions += Instr(
          B,
          BranchLabel(doneLabel),
          EQ
        )
      }
      case Or => {
        irProgram.instructions += Instr(
          MOV,
          scratchRegs(0),
          Imm(1),
          NE
        )
        irProgram.instructions += Instr(
          B,
          BranchLabel(doneLabel),
          NE
        )
      }
    }
    irProgram.instructions += Instr(CMP, scratchRegs(1), Imm(0))
    irProgram.instructions += Instr(
      MOV,
      scratchRegs(0),
      Imm(1),
      NE
    )
    irProgram.instructions += Instr(
      MOV,
      scratchRegs(0),
      Imm(0),
      EQ
    )
    irProgram.instructions += Label(doneLabel)
  }

  private def compareInstruction(op: BinaryOp)(
      implicit irProgram: IRProgram) = {
    irProgram.instructions += Instr(CMP, scratchRegs(0), scratchRegs(1))
    irProgram.instructions += Instr(
      MOV,
      scratchRegs(0),
      Imm(1),
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
      scratchRegs(0),
      Imm(0),
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
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    (op: @unchecked) match {
      case Not => {
        irProgram.instructions += Instr(CMP, paramRegs(0), Imm(0))
        irProgram.instructions += Instr(
          MOV,
          paramRegs(0),
          Imm(0),
          NE
        )
        irProgram.instructions += Instr(
          MOV,
          paramRegs(0),
          Imm(1),
          EQ
        )
      }
      case Negation => {
        irProgram.instructions += Instr(RSBS,
                                        paramRegs(0),
                                        paramRegs(0),
                                        Imm(0))
        irProgram.instructions += Instr(
          BL,
          BranchLabel("error_arithmetic_overflow"),
          VS
        )
      }
      case Len =>
        irProgram.instructions += Instr(BL, BranchLabel("array_size"))
    }
    irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
  }

  private def getTemporaryLabelNameForStringLiterals(labelCount: Int): String =
    s".L.str${labelCount}"

  private def buildStackString(string: String)(
      implicit irProgram: IRProgram) = {
    val label = LabelRef(
      getTemporaryLabelNameForStringLiterals(irProgram.stringLiteralCounter))
    irProgram.instructions += Data(label, string)
    irProgram.stringLiteralCounter += 1
    irProgram.instructions += Instr(LDR, scratchRegs(0), label)
    irProgram.instructions += Instr(PUSH, RegisterList(List(scratchRegs(0))))
  }
  /* Evaluates expression and places result on top of the stack */
  private def buildExpression(
      expr: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    implicit def boolToInt(bool: Boolean): Int = if (bool) 1 else 0
    expr match {
      case IntLiteral(value) => {
        loadMovConstant(scratchRegs(0), value)
        irProgram.instructions += Instr(PUSH,
                                        RegisterList(List(scratchRegs(0))))
      }
      case CharLiteral(char) => {
        irProgram.instructions += Instr(MOV, scratchRegs(0), Imm(char.toInt))
        irProgram.instructions += Instr(PUSH,
                                        RegisterList(List(scratchRegs(0))))
      }
      case BoolLiteral(bool) => {
        irProgram.instructions += Instr(MOV, scratchRegs(0), Imm(bool.toInt))
        irProgram.instructions += Instr(PUSH,
                                        RegisterList(List(scratchRegs(0))))
      }
      case StringLiteral(string) => buildStackString(string)
      case PairLiteral => {
        irProgram.instructions += Instr(MOV, scratchRegs(0), Imm(0))
        irProgram.instructions += Instr(PUSH,
                                        RegisterList(List(scratchRegs(0))))
      }
      case id @ Identifier(_) => {
        irProgram.instructions += Instr(
          getLoadDataOperand(irProgram.symbolTable.lookupType(id)),
          scratchRegs(0),
          AddrReg(FP, irProgram.symbolTable.lookupAddress(id))
        )
        irProgram.instructions += Instr(PUSH,
                                        RegisterList(List(scratchRegs(0))))
      }
      case BinaryOpApp(op, lexpr, rexpr) => {
        buildExpression(lexpr)
        buildExpression(rexpr)
        irProgram.instructions += Instr(POP, RegisterList(List(scratchRegs(1))))
        irProgram.instructions += Instr(POP, RegisterList(List(scratchRegs(0))))
        op match {
          case Plus | Minus | Mul | Div | Mod => nonCompareInstruction(op)
          case And | Or                       => logicalCompareInstruction(op)
          case default                        => compareInstruction(op)
        }
        irProgram.instructions += Instr(PUSH,
                                        RegisterList(List(scratchRegs(0))))
      }
      case UnaryOpApp(op, expr) => {
        buildExpression(expr)
        op match {
          case Not | Negation | Len => modifyingUnaryOp(op)
          case default              => ()
        }
      }
      case ArrayElem(id, indices) => buildArrayAccess(id, indices)
      case default                => throw new Exception("Invalid expression type")
    }
  }

  private def buildArrayLiteral(args: List[Expression], argType: SAType)(
      implicit
      irProgram: IRProgram,
      funcName: String): Unit = {
    /*
    we need to return something that can then be stored in a given variable
    the variable will store the address that has been malloced which will be
    at the top of the stack
    length offset is used to store the length of each array before each elem
     */
    val elementSize = argType match {
      case SAArrayType(arrayType, 1) => getNoBytes(arrayType)
      case SAArrayType(_, _)         => 4
      case _                         => throw new Exception("Unexpected LValue type")
    }

    val dimensions = argType match {
      case SAArrayType(_, n) => n
      case _                 => throw new Exception("Unexpected LValue type")
    }

    loadMovConstant(paramRegs(0), elementSize)
    loadMovConstant(paramRegs(1), args.size)
    loadMovConstant(paramRegs(2), dimensions)
    args.reverse.foreach(buildExpression(_))
    irProgram.instructions += Instr(BL, BranchLabel("array_literal_create"))
    irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
  }

  def buildInlinedFuncPrologue()(implicit irProgram: IRProgram,
                                 funcName: String) = {
    irProgram.instructions += Instr(PUSH, RegisterList(List(FP)))
    buildGCFuncCall()
    // Store frame pointer as pointer to frame pointer on stack
    irProgram.instructions += Instr(ADD, FP, SP, Imm(4))
    val frameSize = irProgram.symbolTable.getFrameSize()
    if (irProgram.symbolTable.getFrameSize() > 0) {
      loadRegConstant(SP, SP, -(frameSize + (frameSize % 4)))
    }
  }

  def renameToJumpAtEndOfInlineFunc(funcName: String, occurrence: Int) = {
    funcName + "__" + occurrence.toString
  }

  private def buildFuncCall(id: Identifier, args: List[Expression])(
      implicit
      irProgram: IRProgram,
      funcName: String,
      inlinedFunctionsAndBodies: Map[
        String,
        (Int, List[Parameter], List[Statement])]) = {
    // build expressions in order, will be reversed on stack
    args.reverse.foreach(buildExpression(_))
    irProgram.funcToLibMap.get(id.name) match {
      case Some(_) => {
        var counter = 0
        args.foreach({ exp =>
          irProgram.instructions += Instr(
            POP,
            RegisterList(List(paramRegs(counter))))
          counter += 1
        })
      }
      case None =>
    }
    if (inlinedFunctionsAndBodies.keySet.contains(id.name)) {
      implicit val funcName = id.name
      irProgram.symbolTable.resetScope()
      (inlinedFunctionsAndBodies.get(id.name): @unchecked) match {
        case Some(value) => {
          value._2.foreach((param: Parameter) =>
            irProgram.symbolTable.encountered(param.identifier))
          buildFuncPrologue(true)
          changeScope(true)
          implicit val inlinedFunc = IsFunctionInlined(true)
          val statementsOfInlineFunc = value._3

          buildInlineFunctionStatements(statementsOfInlineFunc)

          irProgram.instructions += Label(
            renameToJumpAtEndOfInlineFunc(id.name, value._1))
          inlinedFunctionsAndBodies.put(id.name,
                                        (value._1 + 1, value._2, value._3))
        }
      }
      changeScope(false)
    } else {
      irProgram.instructions += Instr(BL, BranchLabel(renameFunc(id.name)))
    }
    irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
  }

  private def buildInlineFunctionStatements(statements: List[Statement])(
      implicit
      irProgram: IRProgram,
      funcName: String,
      inlinedFunctionsAndBodies: Map[String,
                                     (Int, List[Parameter], List[Statement])],
      inlinedFunc: IsFunctionInlined) = {
    implicit var isLastStatement = IsLastStatement(false)
    statements.init.foreach(buildStatement(_))
    statements.last match {
      case ReturnStatement(expr) => {
        isLastStatement = IsLastStatement(true)
      }
      case _ => ()
    }
    buildStatement(statements.last)
  }

  private def buildArrayAccess(id: Identifier, indices: List[Expression])(
      implicit
      irProgram: IRProgram,
      funcName: String): Unit = {
    buildArrayLoadReference(id, indices)
    buildStackDereference(getArrayElemType(id, indices))
  }

  // after this function is called, the pointer to the list will be on the top of the stack
  private def buildArrayLoadReference(id: Identifier,
                                      indices: List[Expression])(
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
        irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(1))))
        buildStackDereference(getArrayElemType(id, next))
        irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
        irProgram.instructions += Instr(BL, BranchLabel("array_access"))
        irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
        buildArrLoadHelper(next)
      }
    }
    buildArrLoadHelper(indices)
  }

  private def isPairRef(argType: SAType): Boolean = argType match {
    case SAPairRefType => true
    case default       => false
  }

  implicit def boolToInt(bool: Boolean): Int = if (bool) 1 else 0
  private def buildNewPair(e1: Expression, e2: Expression, argType: SAType)(
      implicit
      irProgram: IRProgram,
      funcName: String) = {
    buildExpression(e2)
    buildExpression(e1)
    (argType: @unchecked) match {
      case SAPairType(fstType, sndType) => {
        val fstIsRef = isPairRef(fstType)
        val sndIsRef = isPairRef(sndType)
        irProgram.instructions += Instr(MOV, paramRegs(0), Imm(fstIsRef.toInt))
        irProgram.instructions += Instr(MOV, paramRegs(1), Imm(sndIsRef.toInt))
      }
    }
    irProgram.instructions += Instr(BL, BranchLabel("pair_create"))
    irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
  }

  /* Evaluates rvalue and places result on top of the stack */
  private def buildRValue(rvalue: RValue, argType: SAType)(
      implicit
      irProgram: IRProgram,
      funcName: String,
      inlinedFunctionsAndBodies: Map[String,
                                     (Int, List[Parameter], List[Statement])])
    : Unit = {
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

  private def buildExit(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    irProgram.instructions += Instr(BL, BranchLabel("exit"))
  }

  def isLValueReference(lvalue: LValue)(implicit irProgram: IRProgram,
                                        funcName: String) =
    getLValueType(lvalue) match {
      case SAArrayType(_, _) | SAPairType(_, _) => true
      case default                              => false
    }

  private def buildAssignment(lvalue: LValue, rvalue: RValue)(
      implicit
      irProgram: IRProgram,
      funcName: String,
      inlinedFunctionsAndBodies: Map[String,
                                     (Int, List[Parameter], List[Statement])])
    : Unit = {
    val lvalueType = getLValueType(lvalue)
    buildRValue(
      rvalue,
      lvalueType
    )
    lvalue match {
      case id @ Identifier(name) => {
        if (isLValueReference(lvalue)) {
          irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(2))))
          irProgram.instructions += Instr(PUSH,
                                          RegisterList(List(paramRegs(2))))
          buildStackString(name)
          irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(1))))
          loadMovConstant(paramRegs(0), irProgram.symbolTable.lookupScope(id))
          irProgram.instructions += Instr(BL, BranchLabel("root_assignment"));
        }
      }
      case default => ()
    }
    buildLValue(lvalue)
  }

  private def buildIf(
      condition: Expression,
      thenBody: List[Statement],
      elseBody: List[Statement]
  )(implicit irProgram: IRProgram,
    funcName: String,
    inlinedFunctionsAndBodies: Map[String,
                                   (Int, List[Parameter], List[Statement])],
    inlinedFunc: IsFunctionInlined,
    isLastStatement: IsLastStatement): Unit = {
    val elseLabel = getTemporaryLabelName(irProgram.labelCount)
    val ifEndLabel = getTemporaryLabelName(irProgram.labelCount + 1)
    irProgram.labelCount += 2
    buildExpression(condition)
    irProgram.instructions += Instr(POP, RegisterList(List(scratchRegs(0))))
    irProgram.instructions += Instr(CMP, scratchRegs(0), Imm(1))
    irProgram.instructions += Instr(
      B,
      BranchLabel(elseLabel),
      NE
    )
    changeScope(true)
    thenBody.foreach(buildStatement(_))
    changeScope(false)
    irProgram.instructions += Instr(B, BranchLabel(ifEndLabel))
    irProgram.instructions += Label(elseLabel)
    changeScope(true)
    elseBody.foreach(buildStatement(_))
    changeScope(false)
    irProgram.instructions += Label(ifEndLabel)
  }

  private def buildWhile(condition: Expression, body: List[Statement])(
      implicit
      irProgram: IRProgram,
      funcName: String,
      inlinedFunctionsAndBodies: Map[String,
                                     (Int, List[Parameter], List[Statement])],
      inlinedFunc: IsFunctionInlined,
      isLastStatement: IsLastStatement): Unit = {
    val conditionLabel = getTemporaryLabelName(irProgram.labelCount)
    val doneLabel = getTemporaryLabelName(irProgram.labelCount + 1)
    irProgram.labelCount += 2
    irProgram.instructions += Label(conditionLabel)
    buildExpression(condition)
    irProgram.instructions += Instr(POP, RegisterList(List(scratchRegs(0))))
    irProgram.instructions += Instr(CMP, scratchRegs(0), Imm(1))
    irProgram.instructions += Instr(
      B,
      BranchLabel(doneLabel),
      NE
    )
    changeScope(true)
    body.foreach(buildStatement(_))
    changeScope(false)
    irProgram.instructions += Instr(
      B,
      BranchLabel(conditionLabel)
    )
    irProgram.instructions += Label(doneLabel)
  }

  private def getExpressionType(
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
          case _                                       => getExpressionType(lexpr)
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

  private def buildPrint(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    val exprType = getExpressionType(expression)

    // Dereference arrays to access value of data
    exprType match {
      case at @ SAArrayType(_, _) => buildStackDereference(at)
      case _                      => ()
    }
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    irProgram.instructions += Instr(PRINT(exprType))
  }

  private def buildFree(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildExpression(expression)
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    getExpressionType(expression) match {
      case SAArrayType(_, _) =>
        irProgram.instructions += Instr(BL, BranchLabel("array_free"))
      case SAPairType(_, _) =>
        irProgram.instructions += Instr(BL, BranchLabel("pair_free"))
      case unexpected =>
        throw new Exception("Unable to free type: " + unexpected)
    }
  }

  private def buildReturn(
      expression: Expression
  )(implicit irProgram: IRProgram,
    funcName: String,
    inlinedFunc: IsFunctionInlined,
    isLastStatement: IsLastStatement,
    inlinedFunctionsAndBodies: Map[String,
                                   (Int, List[Parameter], List[Statement])])
    : Unit = {
    buildExpression(expression)
    val passReference = expression match {
      case id @ Identifier(_)   => isLValueReference(id)
      case ae @ ArrayElem(_, _) => isLValueReference(ae)
      case default              => false
    }
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    irProgram.instructions += Instr(PUSH, RegisterList(List(paramRegs(0))))
    if (!passReference) {
      irProgram.instructions += Instr(MOV, paramRegs(0), Imm(0))
    }
    buildGCFuncReturn()
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    inlinedFunc match {
      case IsFunctionInlined(true)  => buildFuncEpilogue(true)
      case IsFunctionInlined(false) => buildFuncEpilogue(false)
    }
  }

  private def buildRead(
      lvalue: LValue
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildLValueReference(lvalue)
    irProgram.instructions += Instr(POP, RegisterList(List(paramRegs(0))))
    val noBytes = getNoBytes(getLValueType(lvalue))
    loadMovConstant(paramRegs(1), noBytes);
    irProgram.instructions += Instr(BL, BranchLabel("read"))
  }

  private def buildBegin(
      statements: List[Statement]
  )(implicit irProgram: IRProgram,
    funcName: String,
    inlinedFunctionsAndBodies: Map[String,
                                   (Int, List[Parameter], List[Statement])],
    inlinedFunc: IsFunctionInlined,
    isLastStatement: IsLastStatement): Unit = {
    changeScope(true)
    statements.foreach(buildStatement(_))
    changeScope(false)
  }

  private def buildPrintln(
      expression: Expression
  )(implicit irProgram: IRProgram, funcName: String): Unit = {
    buildPrint(expression)
    irProgram.instructions += Instr(PRINTLN)
  }

  private def buildDeclaration(argType: SAType, id: Identifier, rvalue: RValue)(
      implicit
      irProgram: IRProgram,
      funcName: String,
      inlinedFunctionsAndBodies: Map[
        String,
        (Int, List[Parameter], List[Statement])]) = {
    irProgram.symbolTable.encountered(id)
    buildAssignment(id, rvalue)
  }

  private def buildStatement(
      statement: Statement
  )(implicit irProgram: IRProgram,
    funcName: String,
    inlinedFunctionsAndBodies: Map[String,
                                   (Int, List[Parameter], List[Statement])],
    inlinedFunc: IsFunctionInlined,
    isLastStatement: IsLastStatement): Unit = {
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
      case _                            => throw new Exception("Invalid statement type")
    }
  }

  private def renameFunc(functionName: String) = "wacc_" + functionName

  // calls the runtime function func_return
  private def buildGCFuncCall()(implicit irProgram: IRProgram,
                                funcName: String) = {
    irProgram.instructions += Instr(BL, BranchLabel("func_call"))
  }

  def buildFuncPrologue(inlined: Boolean)(implicit irProgram: IRProgram,
                                          funcName: String) = {
    irProgram.instructions += Instr(
      PUSH,
      RegisterList(List(if (inlined) scratchRegs(0) else LR)))
    irProgram.instructions += Instr(PUSH, RegisterList(List(FP)))
    buildGCFuncCall()
    // Store frame pointer as pointer to frame pointer on stack
    irProgram.instructions += Instr(ADD, FP, SP, Imm(4))
    val frameSize = irProgram.symbolTable.getFrameSize()
    if (irProgram.symbolTable.getFrameSize() > 0) {
      loadRegConstant(SP, SP, -(frameSize + (frameSize % 4)))
    }
  }

  // calls the runtime function func_return
  private def buildGCFuncReturn()(implicit irProgram: IRProgram,
                                  funcName: String) = {
    irProgram.instructions += Instr(BL, BranchLabel("func_return"))
  }

  def buildFuncEpilogue(isInlined: Boolean)(
      implicit irProgram: IRProgram,
      isLastStatement: IsLastStatement,
      funcName: String,
      inlinedFunctionsAndBodies: Map[
        String,
        (Int, List[Parameter], List[Statement])]) = {
    irProgram.instructions += Instr(SUB, SP, FP, Imm(4))
    irProgram.instructions += Instr(POP, RegisterList(List(FP)))
    if (isInlined) {
      irProgram.instructions += Instr(POP, RegisterList(List(scratchRegs(0))))
      isLastStatement match {
        case IsLastStatement(false) =>
          irProgram.instructions += Instr(
            B,
            BranchLabel(
              renameToJumpAtEndOfInlineFunc(
                funcName,
                inlinedFunctionsAndBodies.get(funcName).get._1)))
        case IsLastStatement(true) => ()
      }
    } else {
      irProgram.instructions += Instr(POP, RegisterList(List(PC)))
      irProgram.instructions += Ltorg
    }
  }

  private def buildFunc(
      func: Func
  )(implicit irProgram: IRProgram,
    funcName: String,
    inlinedFunctionsAndBodies: Map[String,
                                   (Int, List[Parameter], List[Statement])]) = {
    if (!inlinedFunctionsAndBodies.keySet.contains(funcName)) {
      implicit val inlinedFunc = IsFunctionInlined(false);
      implicit val isLastStatement = IsLastStatement(false)
      irProgram.instructions += Label(
        renameFunc(func.identBinding.identifier.name)
      )
      irProgram.symbolTable.resetScope()
      func.params.foreach((param: Parameter) =>
        irProgram.symbolTable.encountered(param.identifier))
      buildFuncPrologue(false)
      changeScope(true)
      func.body.foreach(buildStatement(_))
      changeScope(false)
    }
  }

  private def buildFuncs(
      functions: List[Func]
  )(implicit irProgram: IRProgram,
    inlinedFunctionsAndBodies: Map[String,
                                   (Int, List[Parameter], List[Statement])]) = {
    functions.foreach(
      (func: Func) =>
        buildFunc(func)(irProgram,
                        func.identBinding.identifier.name,
                        inlinedFunctionsAndBodies))
  }

  private def buildMain(statements: List[Statement])(
      implicit
      irProgram: IRProgram,
      inlinedFunctionsAndBodies: Map[
        String,
        (Int, List[Parameter], List[Statement])]) = {
    implicit val funcName = "0"
    irProgram.instructions += Global("main")
    irProgram.instructions += Label("main")
    irProgram.symbolTable.resetScope()
    buildFuncPrologue(false)
    changeScope(true)
    implicit val inlinedFunc = IsFunctionInlined(false);
    implicit val isLastStatement = IsLastStatement(false)
    statements.foreach(buildStatement(_))
    changeScope(false)
    irProgram.instructions += Instr(MOV, paramRegs(0), Imm(0))
    buildGCFuncReturn()
    irProgram.instructions += Instr(BL, BranchLabel("gc_free"))
    irProgram.instructions += Instr(MOV, paramRegs(0), Imm(0))
    buildFuncEpilogue(false)
  }

  def buildIR(ast: Program,
              symbolTable: SymbolTable,
              funcToLibMap: Map[String, String]): ListBuffer[IRType] = {
    implicit val inlinedFunctionsAndBodies = getInlinedFunctions(ast)
    implicit val irProgram =
      IRProgram(ListBuffer(), 0, 0, symbolTable, funcToLibMap)
    buildFuncs(ast.functions)
    buildMain(ast.statements)
    irProgram.instructions
  }
}
