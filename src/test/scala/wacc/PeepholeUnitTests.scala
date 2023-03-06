package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.immutable.List
import wacc.Peephole.peepholeOptimisation

class PeepholeUnitTests extends AnyFlatSpec {
    "Peephole optimizer" should "correctly optimize consecutive pushes and pops to the same registers" in {
        assert(peepholeOptimisation(List(Instr(PUSH, RegisterList(List(R0))), Instr(POP, RegisterList(List(R0))))) == List())
    }

    "Peephole optimizer" should "correctly optimize consecutive pushes and pops to the same registers with multiple registers" in {
        assert(peepholeOptimisation(List(Instr(PUSH, RegisterList(List(R0, R1, R2, R3, R4, R8, R9, R10, R12, LR, PC, SP))), Instr(POP, RegisterList(List(R0, R1, R2, R3, R4, R8, R9, R10, R12, LR, PC, SP))))) == List())
    }

    "Peephole optimizer" should "correctly optimize consecutive pushes and pops even when there are other instructions" in {
        assert(peepholeOptimisation(List(Instr(PUSH, RegisterList(List(R0))), Instr(POP, RegisterList(List(R0))), Instr(ADD, R0, R1, R2))) == List(Instr(ADD, R0, R1, R2)))
    }

    "Peephole optimizer" should "correctly remove redundant consecutive mov instructions" in {
        assert(peepholeOptimisation(List(Instr(MOV, R0, R1), Instr(MOV, R1, R0))) == List(Instr(MOV, R0, R1)))
    }

    "Peephole optimizer" should "correctly remove redundant consecutive mov instructions even when there are other instructions" in {
        assert(peepholeOptimisation(List(Instr(MOV, R0, R1), Instr(MOV, R1, R0), Instr(ADD, R0, R1, R2))) == List(Instr(MOV, R0, R1), Instr(ADD, R0, R1, R2)))
    }

    "Peephole optimizer" should "correctly remove mov instructions where source and destination are the same" in {
        assert(peepholeOptimisation(List(Instr(MOV, R0, R0))) == List())
    }

    "Peephole optimizer" should "correctly remove redundant ldr instructions with str instructions" in {
        assert(peepholeOptimisation(List(Instr(STR, R0, AddrReg(R1)), Instr(LDR, R0, AddrReg(R1)))) == List(Instr(STR, R0, AddrReg(R1))))
    }

    "Peephole optimizer" should "correctly remove redundant ldr instructions with ldr instructions" in {
        assert(peepholeOptimisation(List(Instr(LDR, R0, AddrReg(R1)), Instr(LDR, R0, AddrReg(R1)))) == List(Instr(LDR, R0, AddrReg(R1))))
    }
}