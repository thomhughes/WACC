object ast {
  case class Program(functions: List[Func], statements: Statement)
  case class Func(typeName: Type, identifier: Identifier, params: List[Parameter], body: Statement)
  case class Parameter(typeName: Type, identifier: Identifier)

  sealed trait Statement
  case object SkipStatement extends Statement
  case class DeclarationStatement(typeName: Type, identifier: Identifier, rvalue: RValue) extends Statement
  case class AssignmentStatement(lvalue: LValue, rvalue: RValue) extends Statement
  case class ReadStatement(lvalue: LValue) extends Statement
  case class FreeStatement(expression: Expression) extends Statement
  case class ReturnStatement(expression: Expression) extends Statement
  case class ExitStatement(expression: Expression) extends Statement
  case class PrintStatement(expression: Expression) extends Statement
  case class PrintLnStatement(expression: Expression) extends Statement
  case class IfStatement(condition: Expression, thenStatement: Statement, elseStatement: Statement) extends Statement
  case class WhileStatement(condition: Expression, doStatement: Statement) extends Statement
  case class BeginStatement(statement: Statement) extends Statement
  case class SequenceStatement(firstStatement: Statement, secondStatement: Statement) extends Statement

  sealed trait PairIndex
  case object Fst extends PairIndex
  case object Snd extends PairIndex 

  sealed trait LValue
  case class Identifier(name: String) extends LValue with Expression
  case class PairElem(pairIndex: PairIndex, value: LValue) extends LValue with RValue
  case class ArrayElem(identifier: Identifier, index: Expression) extends LValue with Expression

  sealed trait Type
  case object IntType extends Type
  case object BoolType extends Type
  case object CharType extends Type
  case object StringType extends Type
  case class ArrayType(arrayType: Type) extends Type
  // in this impl, fstType can be a PairType, is it necessary to stop this
  case class PairType(fstType: Type, sndType: Type) extends Type
  case object PairRef extends Type

  sealed trait UnaryOp
  case object Not extends UnaryOp
  case object Neg extends UnaryOp
  case object Len extends UnaryOp
  case object Ord extends UnaryOp
  case object Chr extends UnaryOp

  sealed trait BinaryOp
  case object Mul extends BinaryOp
  case object Div extends BinaryOp
  case object Mod extends BinaryOp
  case object Plus extends BinaryOp
  case object Minus extends BinaryOp
  case object Greater extends BinaryOp
  case object GreaterThanEqual extends BinaryOp
  case object Less extends BinaryOp
  case object LessThanEqual extends BinaryOp
  case object Equals extends BinaryOp
  case object NotEquals extends BinaryOp
  case object And extends BinaryOp
  case object Or extends BinaryOp

  sealed trait RValue
  case class NewPair(firstExpr: Expression, secondExpr: Expression) extends RValue
  case class ArrayLiteral(expressions: List[Expression]) extends Rvalue
  case class FunctionCall(identifier: Identifier, argList: List[Expression]) extends RValue

  sealed trait Expression extends RValue
  case class IntLiteral(value: Int) extends Expression
  case class BoolLiteral(value: Boolean) extends Expression
  case class CharLiteral(value: Char) extends Expression
  case class StringLiteral(value: String) extends Expression
  case object PairLiteral extends Expression
  case class UnaryOpApp(uOp: UnaryOp, expr: Expression) extends Expression
  case class BinaryOpApp(binaryOp: BinaryOp, firstExpr: Expression, secondExpr: Expression) extends Expression
  case class Parentheses(expr: Expression) extends Expression
}
