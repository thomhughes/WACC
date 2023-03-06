package wacc

object IRToAssemblyConverter {
  import scala.collection.immutable.List
  import wacc.Types._

  // Main function called externally. Passes a strinbuilder around implicitly
  def convertAssembly(
      instructions: List[IRType]
  ): String = {
    implicit val sb = new StringBuilder()
    sb.append(".text\n")
    instructions.foreach(convertInstructionToAssembly)
    sb.toString()
  }

  // Generic function to convert Instrs and other IRTypes
  private def convertInstructionToAssembly(
      instruction: IRType
  )(implicit instrSb: StringBuilder) = {
    instrSb.append(instruction match {
      case Instr(_, _, _, _, _) => "\t"
      case _                    => ""
    })
    instrSb.append(instruction match {
      case Label(label)                 => convertLabelToAssembly(label)
      case Data(LabelRef(label), value) => convertDataToAssembly(label, value)
      case Ltorg                        => f".ltorg"
      case Global(label)                => f".global ${label}"
      case Instr(opcode, None, None, None, _) =>
        generateAssemblyForOpcode(opcode)
      case Instr(opcode, Some(firstOp), None, None, cond) =>
        f"${generateAssemblyForOpcode(opcode)}${generateAssemblyForConditionCode(
          cond)} ${convertOperandToAssembly(firstOp)}"
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

  private def generateAssemblyForOpcode(opcode: Opcode): String = {
    opcode match {
      case MOV             => "mov"
      case LDR             => "ldr"
      case ADD             => "add"
      case POP             => "pop"
      case PUSH            => "push"
      case SUB             => "sub"
      case ADDS            => "adds"
      case SUBS            => "subs"
      case RSBS            => "rsbs"
      case SMULL           => "smull"
      case STR             => "str"
      case STRB            => "strb"
      case CMP             => "cmp"
      case LDRB            => "ldrb"
      case B               => "b"
      case BL              => "bl"
      case PRINT(typeName) => "bl " + getPrintLabelOfTypeName(typeName)
      case READ(typeName)  => "bl " + getReadLabelOfTypeName(typeName)
      case DIV | MOD       => "bl __aeabi_idivmod"
      case PRINTLN         => "bl _println"
      case default =>
        throw new Exception("IR Conversion Error: Invalid opcode type")
    }
  }

  // To select appropriate read (informs no of bytes)
  private def getReadLabelOfTypeName(typeName: SAType): String = {
    typeName match {
      case SAIntType  => "_readi"
      case SACharType => "_readc"
      case _ =>
        throw new Exception("IR Conversion Error: Invalid type name for read")
    }
  }

  private def generateAssemblyForConditionCode(condition: Condition) = {
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

  private def getPrintLabelOfTypeName(typeName: SAType) = {
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

  private def convertLabelToAssembly(label: String): String =
    new StringBuilder(label).append(":").toString()

  private def convertDataToAssembly(label: String, value: String): String = {
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

  private def convertOperandToAssembly(operand: Operand): String = {
    // Helper for assembly conversion
    val convertShiftToAssembly: Shift => String = _ match {
      case Shift(ASR, shiftAmount) => f"asr #$shiftAmount"
      case Shift(LSL, shiftAmount) => f"lsl #$shiftAmount"
      case Shift(LSR, shiftAmount) => f"lsr #$shiftAmount"
      case Shift(ROR, shiftAmount) => f"ror #$shiftAmount"
    }
    operand match {
      case Imm(i)            => f"#$i"
      case LabelRef(name)    => f"=${name}"
      case R0                => "r0"
      case R1                => "r1"
      case R2                => "r2"
      case R3                => "r3"
      case R4                => "r4"
      case R8                => "r8"
      case R9                => "r9"
      case R10               => "r10"
      case SP                => "sp"
      case R12               => "r12"
      case FP                => "fp"
      case PC                => "pc"
      case LR                => "lr"
      case BranchLabel(name) => name
      case RegisterList(registers) =>
        registers
          .map(convertOperandToAssembly)
          .mkString("{", ", ", "}")
      case AddrReg(reg, offset) =>
        f"[${convertOperandToAssembly(reg)}${if (offset != 0) f" , #$offset"
        else ""}]"
      case ShiftedRegister(reg, shift) =>
        f"${convertOperandToAssembly(reg)}, ${convertShiftToAssembly(shift)}"
      case JoinedRegister(lo, hi) =>
        f"${convertOperandToAssembly(lo)}, ${convertOperandToAssembly(hi)}"
    }
  }
}
