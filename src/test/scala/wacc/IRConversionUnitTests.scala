package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.IR._
import wacc.Parser._
import wacc.Types._
import scala.collection.mutable.ListBuffer
import wacc.Analyser.checkProgram
import org.scalatest.BeforeAndAfterEach

class IRConversionUnitTests extends AnyFlatSpec with BeforeAndAfterEach {

    override def beforeEach() = Analyser.symbolTable.map.clear()
    
    "IR Conversion" should "generate the correct IR for an AST for a basic declaration" in {
        val program = parseAsProgram("begin int x = 3 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV, Some(R8), Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a basic reassignment" in {
        val program = parseAsProgram("begin int x = 3; x = 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV, Some(R8), Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x"))), Instr(MOV, Some(R8), Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple addition" in {
        val program = parseAsProgram("begin int x = 3 + 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a complex addition" in {
        val program = parseAsProgram("begin int x = (3 + 4) + (5 + 6) end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(6))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for print statements with string literals" in {
        val program = parseAsProgram("begin print \"Hello\" end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Data(LabelRef(".L.str0"),"Hello"), Instr(LDR,Some(R8),Some(LabelRef(".L.str0"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAStringType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with int literals" in {
        val program = parseAsProgram("begin print 3 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAIntType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with bool literals" in {
        val program = parseAsProgram("begin print true end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SABoolType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with char literals" in {
        val program = parseAsProgram("begin print 'a' end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(97))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SACharType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with binary operator application" in {
        val program = parseAsProgram("begin print 3 + 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAIntType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with variable references" in {
        val program = parseAsProgram("begin int x = 3; print x end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x"))), Instr(LDR,Some(R8),Some(Var("x"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAIntType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with variable references and binary operator applications" in {
        val program = parseAsProgram("begin int x = 3; print x + 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x"))), Instr(LDR,Some(R8),Some(Var("x"))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAIntType))))
    }

    "IR Conversion" should "generate the correct IR for two print statements with different string literals" in {
        val program = parseAsProgram("begin print \"Hello\"; print \"World\" end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Data(LabelRef(".L.str0"),"Hello"), Instr(LDR,Some(R8),Some(LabelRef(".L.str0"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAStringType)), Data(LabelRef(".L.str1"),"World"), Instr(LDR,Some(R8),Some(LabelRef(".L.str1"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAStringType))))
    }

    "IR Conversion" should "generate the correct IR for reassignment" in {
        val program = parseAsProgram("begin int x = 3; x = 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x"))), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple multiplication" in {
        val program = parseAsProgram("begin int x = 3 * 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(MUL,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple division" in {
        val program = parseAsProgram("begin int x = 3 / 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(DIV,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a complex addition and multiplication" in {
        val program = parseAsProgram("begin int x = (3 + 4) * (5 + 6) end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(6))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(MUL,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple new pair declaration" in {
        val program = parseAsProgram("begin pair(int, int) x = newpair(3, 4) end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV, Some(R0), Some(Imm(4))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(MOV, Some(R8), Some(Imm(3))), Instr(PUSH, Some(R8)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(MOV, Some(R0), Some(Imm(4))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(MOV, Some(R8), Some(Imm(4))), Instr(PUSH, Some(R8)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(MOV, Some(R0), Some(Imm(8))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(AddrReg(R12, 4))), Instr(POP, Some(R8)), Instr(SUB, Some(R12), Some(R12), Some(Imm(4))), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a reassignment to a new pair" in {
        val program = parseAsProgram("begin pair(int, int) x = newpair(3, 4); x = newpair(4, 5) end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV, Some(R0), Some(Imm(4))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(MOV, Some(R8), Some(Imm(3))), Instr(PUSH, Some(R8)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(MOV, Some(R0), Some(Imm(4))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(MOV, Some(R8), Some(Imm(4))), Instr(PUSH, Some(R8)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(MOV, Some(R0), Some(Imm(8))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(AddrReg(R12, 4))), Instr(POP, Some(R8)), Instr(SUB, Some(R12), Some(R12), Some(Imm(4))), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x"))), Instr(MOV, Some(R0), Some(Imm(4))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(MOV, Some(R8), Some(Imm(4))), Instr(PUSH, Some(R8)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(MOV, Some(R0), Some(Imm(4))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(MOV, Some(R8), Some(Imm(5))), Instr(PUSH, Some(R8)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(MOV, Some(R0), Some(Imm(8))), Instr(MALLOC), Instr(MOV, Some(R12), Some(R0)), Instr(POP, Some(R8)), Instr(STR, Some(R8), Some(AddrReg(R12, 4))), Instr(POP, Some(R8)), Instr(SUB, Some(R12), Some(R12), Some(Imm(4))), Instr(STR, Some(R8), Some(R12)), Instr(PUSH, Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for an array literal declaration" in {
        val program = parseAsProgram("begin int[] x = [1, 2, 3] end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R0),Some(Imm(16))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(3))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(2))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,8))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for an array literal reassignment" in {
        val program = parseAsProgram("begin int[] x = [1,2,3] ; x[0] = 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R0),Some(Imm(16))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(3))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(2))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,8))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("x"))), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(LDR,Some(R9),Some(Var("x"))), Instr(PUSH,Some(R9)), Instr(MOV,Some(R8),Some(Imm(0))), Instr(PUSH,Some(R8)), Instr(POP,Some(R10)), Instr(POP,Some(R3)), Instr(POP,Some(R8)), Instr(ARRSTORE)))
    }

    "IR Conversion" should "generate the correct IR for an AST for a 2D array literal declaration" in {
        val program = parseAsProgram("begin int[] d1 = [1, 2, 3] ; int[] d2 = [4, 5] ; int[][] d3 = [d1, d2] end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R0),Some(Imm(16))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(3))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(2))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,8))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d1"))), Instr(MOV,Some(R0),Some(Imm(12))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(2))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d2"))), Instr(MOV,Some(R0),Some(Imm(12))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(2))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(LDR,Some(R8),Some(Var("d1"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(LDR,Some(R8),Some(Var("d2"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d3")))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a basic 2D array literal reassignment" in {
        val program = parseAsProgram("begin int[] d1 = [1, 2, 3] ; int[] d2 = [4, 5] ; int[][] d3 = [d1, d2] ; d3[0] = d2 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R0),Some(Imm(16))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(3))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(2))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,8))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d1"))), Instr(MOV,Some(R0),Some(Imm(12))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(2))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d2"))), Instr(MOV,Some(R0),Some(Imm(12))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(2))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(LDR,Some(R8),Some(Var("d1"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(LDR,Some(R8),Some(Var("d2"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d3"))), Instr(LDR,Some(R8),Some(Var("d2"))), Instr(PUSH,Some(R8)), Instr(LDR,Some(R9),Some(Var("d3"))), Instr(PUSH,Some(R9)), Instr(MOV,Some(R8),Some(Imm(0))), Instr(PUSH,Some(R8)), Instr(POP,Some(R10)), Instr(POP,Some(R3)), Instr(POP,Some(R8)), Instr(ARRSTORE)))
    }

    "IR Conversion" should "generate the correct IR for an AST for a 2D array literal reassignment" in {
        val program = parseAsProgram("begin int[] d1 = [1, 2, 3] ; int[] d2 = [4, 5] ; int[][] d3 = [d1, d2] ; d3[0][1] = 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        val (instructions, _) = buildIR(program, symbolTable)
        instructions should be (ListBuffer(Instr(MOV,Some(R0),Some(Imm(16))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(3))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(2))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,8))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d1"))), Instr(MOV,Some(R0),Some(Imm(12))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(2))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d2"))), Instr(MOV,Some(R0),Some(Imm(12))), Instr(MALLOC), Instr(MOV,Some(R12),Some(R0)), Instr(ADD,Some(R12),Some(R12),Some(Imm(4)),AL), Instr(MOV,Some(R8),Some(Imm(2))), Instr(STR,Some(R8),Some(AddrReg(R12,-4))), Instr(LDR,Some(R8),Some(Var("d1"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,0))), Instr(LDR,Some(R8),Some(Var("d2"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(AddrReg(R12,4))), Instr(PUSH,Some(R12)), Instr(POP,Some(R8)), Instr(STR,Some(R8),Some(Var("d3"))), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(LDR,Some(R9),Some(Var("d3"))), Instr(PUSH,Some(R9)), Instr(POP,Some(R3)), Instr(MOV,Some(R8),Some(Imm(0))), Instr(PUSH,Some(R8)), Instr(POP,Some(R10)), Instr(ARRLOAD), Instr(PUSH,Some(R3)), Instr(MOV,Some(R8),Some(Imm(1))), Instr(PUSH,Some(R8)), Instr(POP,Some(R10)), Instr(POP,Some(R3)), Instr(POP,Some(R8)), Instr(ARRSTORE)))
    }
}