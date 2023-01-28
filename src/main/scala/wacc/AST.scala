object ast {
  case class Program(functions: List[Func], statements: List[Statement])
  case class Func(typeName: Type, identifier: Identifier, params: List[Parameter])
  case class Parameter(typeName: Type, identifier: Identifier)

  sealed trait Statement
  case class SkipStatement() extends Statement
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

  sealed trait LValue
  case class Identifier(name: String) extends LValue with Expression
  case class PairElem(elem: String, value: LValue) extends LValue
  case class ArrayElem(identifier: Identifier, index: Expression) extends LValue with Expression

  sealed trait Type
  case class IntType() extends Type
  case class BoolType() extends Type
  case class CharType() extends Type
  case class StringType() extends Type
  case class ArrayType(arrayType: Type) extends Type
  case class PairType(fstType: Type, sndType: Type) extends Type

  sealed trait RValue
  sealed trait Expression extends RValue
  case class IntLiteral(value: Int) extends Expression
  case class BoolLiteral(value: Boolean) extends Expression
  case class CharLiteral(value: Char) extends Expression
  case class StringLiteral(value: String) extends Expression
  case class PairLiteral() extends Expression
  case class NotExpression(expression: Expression) extends Expression
  case class NegationExpression(expression: Expression) extends Expression
  case class LenExpression(expression: Expression) extends Expression
  case class OrdExpression(expression: Expression) extends Expression
  case class ChrExpression(expression: Expression) extends Expression
  case class MulExpression(lhs: Expression, rhs: Expression) extends Expression
  case class DivExpression(lhs: Expression, rhs: Expression) extends Expression
  case class ModExpression(lhs: Expression, rhs: Expression) extends Expression
  case class PlusExpression(lhs: Expression, rhs: Expression) extends Expression
  case class MinusExpression(lhs: Expression, rhs: Expression) extends Expression
  case class GtExpression(lhs: Expression, rhs: Expression) extends Expression
  case class GeExpression(lhs: Expression, rhs: Expression) extends Expression
  case class LtExpression(lhs: Expression, rhs: Expression) extends Expression
  case class LeExpression(lhs: Expression, rhs: Expression) extends Expression
  case class EqExpression(lhs: Expression, rhs: Expression) extends Expression
  case class NeqExpression(lhs: Expression, rhs: Expression) extends Expression
  case class AndExpression(lhs: Expression, rhs: Expression) extends Expression
  case class OrExpression(lhs: Expression, rhs: Expression) extends Expression
}
