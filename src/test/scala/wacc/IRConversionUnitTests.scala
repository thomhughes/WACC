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
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV, Some(R8), Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a basic reassignment" in {
        val program = parseAsProgram("begin int x = 3; x = 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV, Some(R8), Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8)), Instr(MOV, Some(R8), Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple addition" in {
        val program = parseAsProgram("begin int x = 3 + 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a complex addition" in {
        val program = parseAsProgram("begin int x = (3 + 4) + (5 + 6) end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(6))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for print statements with string literals" in {
        val program = parseAsProgram("begin \n print \"Hello\" \n end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Data(LabelRef(".L.str0"),"Hello"), Instr(LDR,Some(R8),Some(LabelRef(".L.str0"))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAStringType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with int literals" in {
        val program = parseAsProgram("begin \n print 3 \n end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAIntType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with char literals" in {
        val program = parseAsProgram("begin \n print 'a' \n end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(97))), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SACharType))))
    }

    "IR Conversion" should "generate the correct IR for print statements with binary operator application" in {
        val program = parseAsProgram("begin \n print 3 + 4 \n end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R0)), Instr(PRINT(SAIntType))))
    }

    // "IR Conversion" should "generate the correct IR for print statements with variable references" in {
    //     val program = parseAsProgram("begin int x = 3; print x end")
    //     val (_, symbolTable, _) = checkProgram(program)
    //     buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8)), Instr(LDR,Some(R0),Some(Var("x"))), Instr(PRINT(SAIntType))))
    // }

    "IR Conversion" should "generate the correct IR for reassignment" in {
        val program = parseAsProgram("begin int x = 3; x = 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple multiplication" in {
        val program = parseAsProgram("begin int x = 3 * 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(MUL,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple division" in {
        val program = parseAsProgram("begin int x = 3 / 4 end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(DIV,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }

    "IR Conversion" should "generate the correct IR for an AST for a complex addition and multiplication" in {
        val program = parseAsProgram("begin int x = (3 + 4) * (5 + 6) end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(MOV,Some(R8),Some(Imm(3))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(4))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(5))), Instr(PUSH,Some(R8)), Instr(MOV,Some(R8),Some(Imm(6))), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(ADD,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R9)), Instr(POP,Some(R8)), Instr(MUL,Some(R8),Some(R8),Some(R9)), Instr(PUSH,Some(R8)), Instr(POP,Some(R8)), Instr(STR,Some(Var("x")),Some(R8))))
    }
}
