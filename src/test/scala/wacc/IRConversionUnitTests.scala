package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.IR._
import wacc.Parser._
import scala.collection.mutable.ListBuffer
import wacc.Analyser.checkProgram
import org.scalatest.BeforeAndAfterEach

class IRConversionUnitTests extends AnyFlatSpec with BeforeAndAfterEach {

    override def beforeEach() = Analyser.symbolTable.map.clear()
    
    "IR Conversion" should "generate the correct IR for an AST for a basic declaration" in {
        val program = parseAsProgram("begin \n int x = 3 \n end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(PUSH,Some(Imm(3)),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(STR,Some(Var("x")),Some(R9),None,AL)))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple addition" in {
        val program = parseAsProgram("begin \n int x = 3 + 4 \n end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(PUSH,Some(Imm(3)),None,None,AL), Instr(PUSH,Some(Imm(4)),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(POP,Some(R8),None,None,AL), Instr(ADD,Some(R8),Some(R8),Some(R9),AL), Instr(PUSH,Some(R8),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(STR,Some(Var("x")),Some(R9),None,AL)))
    }

    "IR Conversion" should "generate the correct IR for an AST for a complex addition" in {
        val program = parseAsProgram("begin int x = (3 + 4) + (5 + 6) end")
        val (_, symbolTable, _) = checkProgram(program)
        buildIR(program, symbolTable) should be (ListBuffer(Instr(PUSH,Some(Imm(3)),None,None,AL), Instr(PUSH,Some(Imm(4)),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(POP,Some(R8),None,None,AL), Instr(ADD,Some(R8),Some(R8),Some(R9),AL), Instr(PUSH,Some(R8),None,None,AL), Instr(PUSH,Some(Imm(5)),None,None,AL), Instr(PUSH,Some(Imm(6)),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(POP,Some(R8),None,None,AL), Instr(ADD,Some(R8),Some(R8),Some(R9),AL), Instr(PUSH,Some(R8),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(POP,Some(R8),None,None,AL), Instr(ADD,Some(R8),Some(R8),Some(R9),AL), Instr(PUSH,Some(R8),None,None,AL), Instr(POP,Some(R9),None,None,AL), Instr(STR,Some(Var("x")),Some(R9),None,AL)))
    }
}
