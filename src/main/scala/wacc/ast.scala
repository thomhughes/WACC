package wacc

object ast {
  import parsley.genericbridges._

  case class Program(functions: List[Func], statement: List[Statement])
  case class Func(typeName: Type, identifier: Identifier, params: List[Parameter], body: List[Statement])
  case class Parameter(typeName: Type, identifier: Identifier)

  sealed trait Statement
  case class DeclarationStatement(typeName: Type, identifier: Identifier, rvalue: RValue) extends Statement
  case class AssignmentStatement(lvalue: LValue, rvalue: RValue) extends Statement
  case class ReadStatement(lvalue: LValue) extends Statement
  case class FreeStatement(expression: Expression) extends Statement
  case class ReturnStatement(expression: Expression) extends Statement
  case class ExitStatement(expression: Expression) extends Statement
  case class PrintStatement(expression: Expression) extends Statement
  case class PrintLnStatement(expression: Expression) extends Statement
  case class IfStatement(condition: Expression, thenStatement: List[Statement], elseStatement: List[Statement]) extends Statement
  case class WhileStatement(condition: Expression, doStatement: List[Statement]) extends Statement
  case class BeginStatement(statement: List[Statement]) extends Statement

  sealed trait PairIndex
  case object Fst extends PairIndex
  case object Snd extends PairIndex

  sealed trait LValue
  case class Identifier(name: String) extends LValue with Expression
  case class PairElem(index: PairIndex, value: LValue) extends LValue with RValue
  case class ArrayElem(identifier: Identifier, indices: List[Expression]) extends LValue with Expression

  sealed trait Type
  sealed trait BaseType extends Type with PairElemType
  sealed trait PairElemType
  case object IntType extends BaseType with ParserBridge0[BaseType]
  case object BoolType extends BaseType with ParserBridge0[BaseType]
  case object CharType extends BaseType with ParserBridge0[BaseType]
  case object StringType extends BaseType with ParserBridge0[BaseType]
  case object PairRefType extends PairElemType with ParserBridge0[PairElemType]
  case class ArrayType(arrayType: Type) extends Type with PairElemType
  case class PairType(fstType: PairElemType, sndType: PairElemType) extends Type

  sealed trait UnaryOp
  case object Not extends UnaryOp with ParserBridge0[UnaryOp]
  case object Negation extends UnaryOp with ParserBridge0[UnaryOp]
  case object Len extends UnaryOp with ParserBridge0[UnaryOp]
  case object Ord extends UnaryOp with ParserBridge0[UnaryOp]
  case object Chr extends UnaryOp with ParserBridge0[UnaryOp]

  sealed trait BinaryOp 
  case object Mul extends BinaryOp
  case object Div extends BinaryOp
  case object Mod extends BinaryOp
  case object Plus extends BinaryOp
  case object Minus extends BinaryOp
  case object Gt extends BinaryOp
  case object Ge extends BinaryOp
  case object Lt extends BinaryOp
  case object Le extends BinaryOp
  case object Eq extends BinaryOp
  case object Neq extends BinaryOp
  case object And extends BinaryOp
  case object Or extends BinaryOp

  sealed trait RValue
  case class NewPair(fst: Expression, snd: Expression) extends RValue
  case class ArrayLiteral(expressions: List[Expression]) extends RValue
  case class FunctionCall(identifier: Identifier, args: List[Expression]) extends RValue

  sealed trait Expression extends RValue
  case class IntLiteral(value: Int) extends Expression
  case class BoolLiteral(value: Boolean) extends Expression
  case class CharLiteral(value: Char) extends Expression
  case class StringLiteral(value: String) extends Expression
  case object PairLiteral extends Expression
  case class UnaryOpApp(op: UnaryOp, expr: Expression) extends Expression
  case class BinaryOpApp(op: BinaryOp, lhs: Expression, rhs: Expression) extends Expression

  // Bridges
  object Program extends ParserBridge2[List[Func], List[Statement], Program]
  object Func extends ParserBridge4[Type, Identifier, List[Parameter], List[Statement], Func]
  object Parameter extends ParserBridge2[Type, Identifier, Parameter]
  
  case object SkipStatement extends Statement with ParserBridge0[Statement]
  object DeclarationStatement extends Statement with ParserBridge3[Type, Identifier, RValue, DeclarationStatement]
  object AssignmentStatement extends Statement with ParserBridge2[LValue, RValue, AssignmentStatement]
  object ReadStatement extends Statement with ParserBridge1[LValue, ReadStatement]
  object FreeStatement extends Statement with ParserBridge1[Expression, FreeStatement]
  object ReturnStatement extends Statement with ParserBridge1[Expression, ReturnStatement]
  object ExitStatement extends Statement with ParserBridge1[Expression, ExitStatement]
  object PrintStatement extends Statement with ParserBridge1[Expression, PrintStatement]
  object PrintLnStatement extends Statement with ParserBridge1[Expression, PrintLnStatement]
  object IfStatement extends Statement with ParserBridge3[Expression, List[Statement], List[Statement], IfStatement]
  object WhileStatement extends Statement with ParserBridge2[Expression, List[Statement], WhileStatement]
  object BeginStatement extends Statement with ParserBridge1[List[Statement], Statement]

  object Identifier extends LValue with Expression with ParserBridge1[String, Identifier]
  object PairElem extends LValue with RValue with ParserBridge2[PairIndex, LValue, LValue with RValue]
  object ArrayElem extends LValue with Expression with ParserBridge2[Identifier, List[Expression], LValue with Expression]

  object ArrayType extends Type with ParserBridge1[Type, ArrayType]
  object PairType extends Type with ParserBridge2[PairElemType, PairElemType, PairType]

  object NewPair extends RValue with ParserBridge2[Expression, Expression, RValue]
  object ArrayLiteral extends RValue with ParserBridge1[List[Expression], RValue]
  object FunctionCall extends RValue with ParserBridge2[Identifier, List[Expression], RValue]

  object IntLiteral extends Expression with ParserBridge1[Int, IntLiteral]
  object BoolLiteral extends Expression with ParserBridge1[Boolean, BoolLiteral]
  object CharLiteral extends Expression with ParserBridge1[Char, CharLiteral]
  object StringLiteral extends Expression with ParserBridge1[String, StringLiteral]

  object UnaryOpApp extends Expression with ParserBridge2[UnaryOp, Expression, UnaryOpApp]
  object BinaryOpApp extends Expression with ParserBridge3[BinaryOp, Expression, Expression, BinaryOpApp]
}
