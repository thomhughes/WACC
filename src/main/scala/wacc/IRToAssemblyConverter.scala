package wacc

import wacc.SymbolTable
object IRToAssemblyConverter {
  import scala.collection.mutable.ListBuffer
  import wacc.Types._
  import PrintAssemblyGenerator._
  import ReadAssemblyGenerator._
  import FreeAssemblyGenerator._
  import ArrayAssemblyGenerator._

  def convertAssembly(
      instructions: ListBuffer[IRType],
      symbolTable: SymbolTable
  ): String = {
    implicit val sb = new StringBuilder()
    sb.append(".text\n")
    instructions.foreach(convertInstructionToAssembly)
    generatePrintAssemblyForString()
    generatePrintAssemblyForBool()
    generatePrintAssemblyForPointerTypes()
    generatePrintAssemblyForChar()
    generatePrintAssemblyForInt()
    generatePrintAssemblyForPrintLn()
    generateReadAssemblyForInt()
    generateReadAssemblyForChar()
    generateFreeAssemblyForPair()
    generateArrloadAssembly()
    generateArrstoreAssembly()
    generateBoundsCheck()
    sb.toString()
  }

  def convertInstructionToAssembly(
      instruction: IRType
  )(implicit instrSb: StringBuilder) = {
    //println(instruction)
    instrSb.append(instruction match {
      case Instr(_, _, _, _, _) => "\t"
      case _                    => ""
    })
    instrSb.append(instruction match {
      case Label(label)                 => convertLabelToAssembly(label)
      case Data(LabelRef(label), value) => convertDataToAssembly(label, value)
      case Ltorg => f".ltorg"
      case Global(label) => f".global ${label}"
      case Instr(opcode, None, None, None, _) =>
        generateAssemblyForOpcode(opcode)
      case Instr(B, Some(LabelRef(label)), None, None, cond) =>
        f"b${generateAssemblyForConditionCode(cond)} ${label}"
      case Instr(BL, Some(LabelRef(label)), None, None, cond) =>
        f"bl${generateAssemblyForConditionCode(cond)} ${label}"
      case Instr(BX, Some(LabelRef(label)), None, None, cond) =>
        f"bx${generateAssemblyForConditionCode(cond)} ${label}"
      case Instr(POP, Some(firstOp), None, None, _) =>
        f"pop {${convertOperandToAssembly(firstOp)}}"
      case Instr(PUSH, Some(firstOp), None, None, _) =>
        f"push {${convertOperandToAssembly(firstOp)}}"
      case Instr(DIV, Some(firstOp), Some(secondOp), Some(thirdOp), _) =>
        f"mov r0, ${convertOperandToAssembly(secondOp)}\n\tmov r1, ${convertOperandToAssembly(thirdOp)}\n\tbl __aeabi_idivmod\n\tmov ${convertOperandToAssembly(firstOp)}, r0"
      case Instr(MOD, Some(firstOp), Some(secondOp), Some(thirdOp), _) =>
        f"mov r0, ${convertOperandToAssembly(secondOp)}\n\tmov r1, ${convertOperandToAssembly(thirdOp)}\n\tbl __aeabi_idivmod\n\tmov ${convertOperandToAssembly(firstOp)}, r1"
      case Instr(opcode, Some(firstOp), None, None, cond) =>
        f"${generateAssemblyForOpcode(opcode)}${generateAssemblyForConditionCode(cond)} ${convertOperandToAssembly(firstOp)}"
      case Instr(opcode, Some(firstOp), Some(secondOp), None, cond) =>
        f"${generateAssemblyForOpcode(opcode)}${generateAssemblyForConditionCode(
            cond
          )} ${convertOperandToAssembly(firstOp)}, ${convertOperandToAssembly(secondOp)}"
      case Instr(opcode, Some(firstOp), Some(secondOp), Some(thirdOp), cond) =>
        f"${generateAssemblyForOpcode(opcode)}${generateAssemblyForConditionCode(cond)} ${convertOperandToAssembly(
            firstOp
          )}, ${convertOperandToAssembly(secondOp)}, ${convertOperandToAssembly(thirdOp)}"
      case default =>
        throw new Exception("IR Conversion Error: Invalid instruction type")
    })
    instrSb.append("\n")
  }

  def generateAssemblyForOpcode(opcode: Opcode): String = {
    opcode match {
      case MOV             => "mov"
      case LDR             => "ldr"
      case ADD             => "add"
      case SUB             => "sub"
      case MUL             => "mul"
      case MOD             => "mod"
      case AND             => "and"
      case ORR             => "orr"
      case RSB             => "rsb"
      case STR             => "str"
      case STRB            => "strb"
      case CMP             => "cmp"
      case MVN             => "mvn"
      case TST             => "tst"
      case CMN             => "cmn"
      case TEQ             => "teq"
      case LDRB            => "ldrb"
      case PRINT(typeName) => "bl " + getPrintLabelOfTypeName(typeName)
      case READ(typeName)  => "bl " + getReadLabelOfTypeName(typeName)
      case PRINTLN         => "bl _println"
      case MALLOC          => "bl malloc"
      case FREE(typeName)  => "bl " + getFreeLabelOfTypeName(typeName)
      case default =>
        throw new Exception("IR Conversion Error: Invalid opcode type")
    }
  }

  def getFreeLabelOfTypeName(typeName: SAType): String = {
    typeName match {
      case SAPairType(_, _)  => "_freepair"
      case SAArrayType(_, _) => "free"
      case _ => throw new Exception("IR-To-Assembly Error: Invalid opcode type")
    }
  }

  def getReadLabelOfTypeName(typeName: SAType): String = {
    typeName match {
      case SAIntType  => "_readi"
      case SACharType => "_readc"
      case _ =>
        throw new Exception("IR Conversion Error: Invalid type name for read")
    }
  }

  def generateAssemblyForConditionCode(condition: Condition) = {
    condition match {
      case EQ => "eq"
      case NE => "ne"
      case GE => "ge"
      case GT => "gt"
      case LE => "le"
      case LT => "lt"
      case HS => "hs"
      case LO => "lo"
      case CS => "cs"
      case CC => "cc"
      case MI => "mi"
      case PL => "pl"
      case VS => "vs"
      case VC => "vc"
      case HI => "hi"
      case LS => "ls"
      case AL => ""
      case default =>
        throw new Exception("IR Conversion Error: Invalid condition type")
    }
  }

  def getPrintLabelOfTypeName(typeName: SAType) = {
    typeName match {
      case SAIntType                                 => "_printi"
      case SAStringType | SAArrayType(SACharType, 1) => "_prints"
      case SABoolType                                => "_printb"
      case SACharType                                => "_printc"
      case SAPairType(_, _) | SAArrayType(_, _)      => "_printp"
      case default =>
        throw new Exception("Print not supported for type: " + typeName)
    }
  }

  def generatePrintAssemblyForTypeName(
      typeName: SAType
  )(implicit sb: StringBuilder) =
    typeName match {
      case SAStringType => generatePrintAssemblyForString()
      case SAIntType    => generatePrintAssemblyForInt()
      case SABoolType   => generatePrintAssemblyForBool()
      case SACharType   => generatePrintAssemblyForChar()
      case SAPairType(_, _) | SAArrayType(_, _) =>
        generatePrintAssemblyForPointerTypes()
      case default =>
        throw new Exception("Print not supported for type: " + typeName)
    }

  def convertLabelToAssembly(label: String): String =
    new StringBuilder(label).append(":").toString()

  def convertDataToAssembly(label: String, value: String): String = {
    val dataSb = new StringBuilder()
    dataSb
      .append(".data\n\t.word ")
      .append(value.length())
      .append("\n")
      .append(convertLabelToAssembly(label))
      .append("\n\t.asciz \"")
      .append(value.replace("\"", "\\\"").replace("\n", "\\n"))
      .append("\"\n.text")
      .toString()
  }

  def convertOperandToAssembly(operand: Operand): String = {
    operand match {
      case Imm(i)         => f"#$i"
      case LabelRef(name) => f"=${name}"
      case R0             => "r0"
      case R1             => "r1"
      case R2             => "r2"
      case R3             => "r3"
      case R4             => "r4"
      case R8             => "r8"
      case R9             => "r9"
      case R10            => "r10"
      case SP             => "sp"
      case R12            => "r12"
      case FP             => "fp"
      case PC             => "pc"
      case LR             => "lr"
      case AddrReg(reg, offset) =>
        f"[${convertOperandToAssembly(reg)}${if (offset != 0) f" , #$offset"
          else ""}]"
    }
  }
}

object PrintAssemblyGenerator {
  def generatePrintAssemblyForPrintLn()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 0")
      .append("\n.L._println_str0:")
      .append("\n\t.asciz \"\"")
      .append("\n.text")
      .append("\n_println:")
      .append("\n\tpush {lr}")
      .append("\n\tldr r0, =.L._println_str0")
      .append("\n\tbl puts")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tpop {pc}\n")

  def generatePrintAssemblyForInt()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 2")
      .append("\n.L._printi_str0:")
      .append("\n\t.asciz \"%d\"")
      .append("\n.text")
      .append("\n_printi:")
      .append("\n\tpush {lr}")
      .append("\n\tmov r1, r0")
      .append("\n\tldr r0, =.L._printi_str0")
      .append("\n\tbl printf")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tpop {pc}\n")

  def generatePrintAssemblyForString()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 4")
      .append("\n.L._prints_str0:")
      .append("\n\t.asciz \"%.*s\"")
      .append("\n.text")
      .append("\n_prints:")
      .append("\n\tpush {lr}")
      .append("\n\tmov r2, r0")
      .append("\n\tldr r1, [r0, #-4]")
      .append("\n\tldr r0, =.L._prints_str0")
      .append("\n\tbl printf")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tpop {pc}\n")

  def generatePrintAssemblyForChar()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 2")
      .append("\n.L._printc_str0:")
      .append("\n\t.asciz \"%c\"")
      .append("\n.text")
      .append("\n_printc:")
      .append("\n\tpush {lr}")
      .append("\n\tmov r1, r0")
      .append("\n\tldr r0, =.L._printc_str0")
      .append("\n\tbl printf")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tpop {pc}\n")

  def generatePrintAssemblyForBool()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 5")
      .append("\n.L._printb_str0:")
      .append("\n\t.asciz \"false\"")
      .append("\n\t.word 4")
      .append("\n.L._printb_str1:")
      .append("\n\t.asciz \"true\"")
      .append("\n\t.word 4")
      .append("\n.L._printb_str2:")
      .append("\n\t.asciz \"%.*s\"")
      .append("\n.text")
      .append("\n_printb:")
      .append("\n\tpush {lr}")
      .append("\n\tcmp r0, #0")
      .append("\n\tbne .L_printb0")
      .append("\n\tldr r2, =.L._printb_str0")
      .append("\n\tb .L_printb1")
      .append("\n.L_printb0:")
      .append("\n\tldr r2, =.L._printb_str1")
      .append("\n.L_printb1:")
      .append("\n\tldr r1, [r2, #-4]")
      .append("\n\tldr r0, =.L._printb_str2")
      .append("\n\tbl printf")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tpop {pc}\n")

  def generatePrintAssemblyForPointerTypes()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\t\n.word 2")
      .append("\n.L._printp_str0:")
      .append("\n\t.asciz \"%p\"")
      .append("\n.text")
      .append("\n_printp:")
      .append("\n\tpush {lr}")
      .append("\n\tmov r1, r0")
      .append("\n\tldr r0, =.L._printp_str0")
      .append("\n\tbl printf")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tpop {pc}\n")
}

object ReadAssemblyGenerator {
  def generateReadAssemblyForChar()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 3")
      .append("\n.L._readc_str0:")
      .append("\n\t.asciz \" %c\"")
      .append("\n.text")
      .append("\n_readc:")
      .append("\n\tpush {lr}")
      .append("\n\tstrb r0, [sp, #-1]! @ push {r0}")
      .append("\n\tmov r1, sp")
      .append("\n\tldr r0, =.L._readc_str0")
      .append("\n\tbl scanf")
      .append("\n\tldrsb r0, [sp, #0]")
      .append("\n\tadd sp, sp, #1")
      .append("\n\tpop {pc}\n")

  def generateReadAssemblyForInt()(implicit sb: StringBuilder) =
    sb.append(".data")
      .append("\n\t.word 2")
      .append("\n.L._readi_str0:")
      .append("\n\t.asciz \"%d\"")
      .append("\n.text")
      .append("\n_readi:")
      .append("\n\tpush {lr}")
      .append("\n\tstr r0, [sp, #-4]! @ push {r0}")
      .append("\n\tmov r1, sp")
      .append("\n\tldr r0, =.L._readi_str0")
      .append("\n\tbl scanf")
      .append("\n\tldr r0, [sp, #0]")
      .append("\n\tadd sp, sp, #4")
      .append("\n\tpop {pc}\n")
}

object FreeAssemblyGenerator {
  def generateFreeAssemblyForPair()(implicit sb: StringBuilder) =
    sb.append("_freepair:")
      .append("\n\tpush {lr}")
      .append("\n\tmov r8, r0")
      .append("\n\tcmp r8, #0")
      .append("\n\tbleq _errNull")
      .append("\n\tldr r0, [r8, #0]")
      .append("\n\tbl free")
      .append("\n\tldr r0, [r8, #4]")
      .append("\n\tbl free")
      .append("\n\tmov r0, r8")
      .append("\n\tbl free")
      .append("\n\tpop {pc}")
      .append("\n.data")
      .append("\n\t.word 45")
      .append("\n.L._errNull_str0:")
      .append("\n\t.asciz \"fatal error: null pair dereferenced or freed\"\n")
      .append("\n.text")
      .append("\n_errNull:")
      .append("\n\tldr r0, =.L._errNull_str0")
      .append("\n\tbl _prints")
      .append("\n\tmov r0, #255")
      .append("\n\tbl exit\n")
}

object ArrayAssemblyGenerator {
  def generateArrloadAssembly()(implicit sb: StringBuilder) = {
    sb.append("_arrload:")
      .append("\n\tpush {lr}")
      .append("\n\tcmp r10, #0")
      .append("\n\tmovlt r1, #10")
      .append("\n\tbllt _boundsCheck")
      .append("\n\tldr lr, [r3, #-4]")
      .append("\n\tcmp r10, lr")
      .append("\n\tmovge r1, r10")
      .append("\n\tblge _boundsCheck")
      .append("\n\tldr r3, [r3, r10, lsl #2]")
      .append("\n\tpop {pc}\n")
  }

  def generateArrstoreAssembly()(implicit sb: StringBuilder) = {
    sb.append("_arrstore:")
      .append("\n\tpush {lr}")
      .append("\n\tcmp r10, #0")
      .append("\n\tmovlt r1, #10")
      .append("\n\tbllt _boundsCheck")
      .append("\n\tldr lr, [r3, #-4]")
      .append("\n\tcmp r10, lr")
      .append("\n\tmovge r1, r10")
      .append("\n\tblge _boundsCheck")
      .append("\n\tstr r3, [r8, r10, lsl #2]")
      .append("\n\tpop {pc}\n")
  }

  def generateBoundsCheck()(implicit sb: StringBuilder) = {
    sb.append(".data")
      .append("\n\t.word 42")
      .append("\n.L._boundsCheck_str0:")
      .append("\n\t.asciz \"fatal error: array index %d out of bounds\"\n")
      .append("\n.text\n")
      .append("_boundsCheck:")
      .append("\n\tldr r0, =.L._boundsCheck_str0")
      .append("\n\tbl printf")
      .append("\n\tmov r0, #0")
      .append("\n\tbl fflush")
      .append("\n\tmov r0, #255")
      .append("\n\tbl exit\n")
  }
}
