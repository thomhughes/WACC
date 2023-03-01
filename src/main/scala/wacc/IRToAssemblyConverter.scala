package wacc

object IRToAssemblyConverter {
    import scala.collection.mutable.ListBuffer
    import wacc.Types._

    def convertAssembly(instructions: ListBuffer[IRType], symbolTable: SymbolTable): String = {
        implicit val sb = new StringBuilder()
        setupStartOfMain()
        instructions.foreach(convertInstructionToAssembly)
        setupEndOfMain()
        sb.append(getPrintAssemblyForTypeName(SAStringType))
        sb.toString()
    }

    def convertInstructionToAssembly(instruction: IRType)(implicit instrSb: StringBuilder) = {
        println(instruction)
        instrSb.append(instruction match {
            case Instr(_, _, _, _, _) => "\t"
            case _ => ""
        })
        instrSb.append(instruction match {
            case Label(label) => convertLabelToAssembly(label)
            case Data(LabelRef(label), value) => convertDataToAssembly(label, value)
            case Instr(MOV, Some(firstOp), Some(secondOp), None, _) => f"mov ${convertOperandToAssembly(firstOp)}, ${convertOperandToAssembly(secondOp)}"
            case Instr(PUSH, Some(firstOp), None, None, _) => f"push {${convertOperandToAssembly(firstOp)}}"
            case Instr(POP, Some(firstOp), None, None, _) => f"pop {${convertOperandToAssembly(firstOp)}}"
            case Instr(LDR, Some(firstOp), Some(secondOp), None, _) => f"ldr ${convertOperandToAssembly(firstOp)}, ${convertOperandToAssembly(secondOp)}"
            case Instr(PRINT(typeName), None, None, None, _) => f"bl ${getPrintLabelOfTypeName(typeName)}"
            case default => throw new Exception("IR Conversion Error: Invalid instruction type")
        })
        instrSb.append("\n")
    }

    def setupStartOfMain()(implicit instrSb: StringBuilder) {
        instrSb.append(".text\n")
        instrSb.append(".global main\n")
        instrSb.append("main:\n")
        instrSb.append("\tpush {fp, lr}\n")
    }

    def setupEndOfMain()(implicit instrSb: StringBuilder) {
        instrSb.append("\tpop {fp, pc}\n")
    }

    def getPrintLabelOfTypeName(typeName: SAType) = {
        typeName match {
            case SAIntType => "_printi"
            case SAStringType => "_prints"
            case SABoolType => "_printb"
            case SACharType => "_printc"
            case SAPairType(_, _) => "_printp"
            case default => ???
        }
    }

    def getPrintAssemblyForTypeName(typeName: SAType) = {
        typeName match {
            case SAStringType => 
                ".data\n\t.word 4\n.L._prints_str0:\n\t.asciz \"%.*s\"\n.text" +
                  "\n_prints:\n\tpush {lr}\n\tmov r2, r0\n\tldr r1, [r0, #-4]\n\tldr r0, =.L._prints_str0" +
                  "\n\tbl printf\n\tmov r0, #0\n\tbl fflush\n\tpop {pc}"
            case default => ???
        }
    }

    def convertLabelToAssembly(label: String): String = new StringBuilder(label).append(":").toString()

    def convertDataToAssembly(label: String, value: String): String = {
        val dataSb = new StringBuilder()
        dataSb
        .append(".data\n\t.word ")
        .append(value.length())
        .append("\n")
        .append(convertLabelToAssembly(label))
        .append("\n\t.asciz \"")
        .append(value)
        .append("\"\n.text")
        .toString()
    }

    def convertOperandToAssembly(operand: Operand): String = {
        operand match {
            case Imm(i) => f"#$i"
            case Var(name) => ???
            case LabelRef(name) => f"=${name}"
            case ArrayToStore(args) => ???
            case R0 => "r0"
            case R8 => "r8"
            case R9 => "r9"
            case SP => "sp"
            case R12 => "r12"
            case FP => "fp"
        }
    }
}
