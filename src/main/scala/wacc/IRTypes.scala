package wacc

sealed trait IRType
case class Label(name: String) extends IRType 
case class EnterScope(scopeNo: Int) extends IRType
case class ExitScope(scopeNo: Int) extends IRType
case class Instr(opcode: Opcode,
                 op1: Option[Operand] = None,
                 op2: Option[Operand] = None,
                 op3: Option[Operand] = None,
                 cond: Condition = AL)
    extends IRType


sealed trait Operand
case class Imm(int: Int) extends Operand
case class Var(name: String) extends Operand
case class StrLit(str: String) extends Operand
case class CharLit(int: Int) extends Operand
case class LabelRef(name: String) extends Operand
case class ArrayLit(name: String, pos: List[Int]) extends Operand

sealed trait Register extends Operand
case object R0 extends Register
case object R8 extends Register
case object SP extends Register
case object FP extends Register


sealed trait Opcode

sealed trait DataProcessing extends Opcode
case object ADD extends DataProcessing
case object SUB extends DataProcessing
case object MUL extends DataProcessing
case object DIV extends DataProcessing
case object MOD extends DataProcessing
case object AND extends DataProcessing
case object ORR extends DataProcessing
case object MOV extends DataProcessing
case object MVN extends DataProcessing
case object CMP extends DataProcessing
case object CMN extends DataProcessing
case object TST extends DataProcessing
case object TEQ extends DataProcessing

sealed trait MemAccess extends Opcode
case object LDR extends MemAccess
case object STR extends MemAccess
case object LDRB extends MemAccess
case object STRB extends MemAccess
case object LDRH extends MemAccess
case object STRH extends MemAccess
case object LDM extends MemAccess
case object STM extends MemAccess

sealed trait Branch extends Opcode
case object B extends Branch
case object BL extends Branch
case object BX extends Branch

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


sealed trait BuiltInInstruction extends Opcode
case object PRINT extends BuiltInInstruction
case object PRINTLN extends BuiltInInstruction
case object FREE extends BuiltInInstruction
