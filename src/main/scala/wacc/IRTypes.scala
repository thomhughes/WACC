package wacc

import scala.collection.mutable.ListBuffer
import wacc.Types.SAType

import wacc.SymbolTable

case class IRProgram(
    val instructions: ListBuffer[IRType],
    var stringLiteralCounter: Int,
    var labelCount: Int,
    val symbolTable: SymbolTable
)

sealed trait IRType

case class Label(name: String) extends IRType
class Instr private (
    val opcode: Opcode,
    val op1: Option[Operand] = None,
    val op2: Option[Operand] = None,
    val op3: Option[Operand] = None,
    val cond: Condition = AL
) extends IRType {
  override def toString(): String = {
    val sb = new StringBuilder()
    sb ++= "Instr("
    sb ++= (op1 match {
      case Some(op) => op.toString + ","
      case None     => ""
    })
    sb ++= (op2 match {
      case Some(op) => op.toString + ","
      case None     => ""
    })
    sb ++= (op3 match {
      case Some(op) => op.toString + ","
      case None     => ""
    })
    sb ++= (cond match {
      case AL => ""
      case _  => cond.toString + ","
    })
    sb ++= ")"
    sb.toString()
  }
}

// Overloaded to enforce correctness of no arguments and type of arguments
object Instr {
  // SMULL
  def apply(opcode: Mul, op1: JoinedRegister, op2: JoinedRegister): Instr =
    new Instr(SMULL, Some(op1), Some(op2))

  // Arithmetic operations
  def apply(opcode: Arithmetic,
            rd: Register,
            rn: Register,
            op2: Operand): Instr =
    new Instr(opcode, Some(rd), Some(rn), Some(op2))

  // Move opertations
  def apply(opcode: Move, rd: Register, op2: Operand): Instr =
    new Instr(opcode, Some(rd), Some(op2))

  def apply(opcode: Move,
            rd: Register,
            op2: Operand,
            condition: Condition): Instr =
    new Instr(opcode, Some(rd), Some(op2), cond = condition)

  // CMP
  def apply(opcode: Compare, rn: Register, op2: Operand): Instr =
    new Instr(opcode, Some(rn), Some(op2))

  def apply(opcode: StackInstr, r: RegisterList): Instr =
    new Instr(opcode, Some(r))

  // Memory access
  def apply(opcode: MemAccess, r: Register, sr: Operand): Instr =
    new Instr(opcode, Some(r), Some(sr))

  def apply(opcode: Branch, label: BranchLabel): Instr =
    new Instr(opcode, Some(label))

  def apply(opcode: Branch, label: BranchLabel, condition: Condition): Instr =
    new Instr(opcode, Some(label), cond = condition)

  // Buil-in instructions (deferring to IRToAssembly/Runtime)
  def apply(opcode: BuiltInInstruction): Instr =
    new Instr(opcode)

  // unapply defined to allow pattern matching
  def unapply(instr: Instr): Option[
    (Opcode, Option[Operand], Option[Operand], Option[Operand], Condition)] =
    Some((instr.opcode, instr.op1, instr.op2, instr.op3, instr.cond))
}

case class Data(name: LabelRef, value: String) extends IRType

sealed trait ShiftType
case object ASR extends ShiftType
case object LSL extends ShiftType
case object LSR extends ShiftType
case object ROR extends ShiftType
case class Shift(shiftType: ShiftType, shiftAmount: Int)

sealed trait Operand
case class Imm(int: Int) extends Operand
case class LabelRef(name: String) extends Operand
case class JoinedRegister(lo: Register, hi: Register)
    extends Operand
case class ShiftedRegister(reg: Register, shift: Shift)
    extends Operand
case class RegisterList(registers: List[Register]) extends Operand
case class BranchLabel(name: String) extends Operand

sealed trait Register extends Operand
case object R0 extends Register
case object R1 extends Register
case object R2 extends Register
case object R3 extends Register
case object R4 extends Register
case object R8 extends Register
case object R9 extends Register
case object R10 extends Register
case object SP extends Register
case object R12 extends Register
case object FP extends Register
case object LR extends Register
case object PC extends Register

sealed trait Address extends Operand
case class AddrReg(reg: Register, offset: Int = 0) extends Address

sealed trait Opcode

sealed trait Mul extends Opcode
case object SMULL extends Mul

sealed trait Arithmetic extends Opcode
case object ADD extends Arithmetic
case object SUB extends Arithmetic
case object ADDS extends Arithmetic
case object SUBS extends Arithmetic
case object RSBS extends Arithmetic

sealed trait Move extends Opcode
case object MOV extends Move
case object MOVB extends Move

sealed trait Compare extends Opcode
case object CMP extends Compare

sealed trait StackInstr extends Opcode
case object PUSH extends StackInstr
case object POP extends StackInstr

sealed trait MemAccess extends Opcode
case object LDR extends MemAccess
case object LDRB extends MemAccess
case object STR extends MemAccess
case object STRB extends MemAccess

sealed trait Branch extends Opcode
case object B extends Branch
case object BL extends Branch

sealed trait Condition
case object EQ extends Condition
case object NE extends Condition
case object HS extends Condition
case object CS extends Condition
case object LO extends Condition
case object CC extends Condition
case object MI extends Condition
case object PL extends Condition
case object VS extends Condition
case object VC extends Condition
case object HI extends Condition
case object LS extends Condition
case object GE extends Condition
case object LT extends Condition
case object GT extends Condition
case object LE extends Condition
case object AL extends Condition

sealed trait Directive extends IRType
case object Ltorg extends Directive
case class Global(label: String) extends Directive

sealed trait BuiltInInstruction extends Opcode
case class PRINT(saType: SAType) extends BuiltInInstruction
case class READ(saType: SAType) extends BuiltInInstruction
case object PRINTLN extends BuiltInInstruction
case object DIV extends BuiltInInstruction
case object MOD extends BuiltInInstruction
