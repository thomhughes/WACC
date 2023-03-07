package wacc

import parsley.Parsley

object AST {
  import parsley.genericbridges._
  import parsley.position.pos
  import parsley.implicits.zipped._
  import parsley.errors.combinator.ErrorMethods

  case class Program(val imports: List[Import], val functions: List[Func], val statements: List[Statement])(
      val pos: (Int, Int))
  case class Import(val importName: String)(val pos: (Int, Int))
  case class Func(identBinding: IdentBinding,
                  params: List[Parameter],
                  body: List[Statement])(val pos: (Int, Int))
  case class Parameter(typeName: Type, identifier: Identifier)(
      val pos: (Int, Int))
  case class IdentBinding(typeName: Type, identifier: Identifier)(
      val pos: (Int, Int))

  sealed trait Statement
  case class DeclarationStatement(typeName: Type,
                                  val identifier: Identifier,
                                  rvalue: RValue)(val pos: (Int, Int))
      extends Statement
  case class AssignmentStatement(lvalue: LValue, rvalue: RValue)(
      val pos: (Int, Int))
      extends Statement
  case class ReadStatement(lvalue: LValue)(val pos: (Int, Int))
      extends Statement
  case class FreeStatement(expression: Expression)(val pos: (Int, Int))
      extends Statement
  case class ReturnStatement(expression: Expression)(val pos: (Int, Int))
      extends Statement
  case class ExitStatement(expression: Expression)(val pos: (Int, Int))
      extends Statement
  case class PrintStatement(expression: Expression)(val pos: (Int, Int))
      extends Statement
  case class PrintLnStatement(expression: Expression)(val pos: (Int, Int))
      extends Statement
  case class IfStatement(condition: Expression,
                         thenStatement: List[Statement],
                         elseStatement: List[Statement])(val pos: (Int, Int))
      extends Statement
  case class WhileStatement(condition: Expression, doStatement: List[Statement])(
      val pos: (Int, Int))
      extends Statement
  case class BeginStatement(statement: List[Statement])(val pos: (Int, Int))
      extends Statement

  sealed trait PairIndex
  case object Fst extends PairIndex with ParserBridge0[PairIndex]
  case object Snd extends PairIndex with ParserBridge0[PairIndex]

  sealed trait LValue
  case class Identifier(name: String)(val pos: (Int, Int))
      extends LValue
      with Expression
  case class PairElem(index: PairIndex, value: LValue)(val pos: (Int, Int))
      extends LValue
      with RValue
  case class ArrayElem(identifier: Identifier, indices: List[Expression])(
      val pos: (Int, Int))
      extends LValue
      with Expression

  sealed trait ASTType
  sealed trait Type extends ASTType
  sealed trait BaseType extends Type with PairElemType
  sealed trait PairElemType extends ASTType
  case object NoneType extends Type with ParserBridge0[Type]
  case object IntType extends BaseType with ParserBridge0[BaseType]
  case object BoolType extends BaseType with ParserBridge0[BaseType]
  case object CharType extends BaseType with ParserBridge0[BaseType]
  case object StringType extends BaseType with ParserBridge0[BaseType]
  case object PairRefType extends PairElemType with ParserBridge0[PairElemType]
  case class ArrayType(arrayType: Type, arity: Int)(val pos: (Int, Int))
      extends Type
      with PairElemType
  case class PairType(fstType: PairElemType, sndType: PairElemType)(
      val pos: (Int, Int))
      extends Type

  sealed trait UnaryOp
  case object Not extends UnaryOp
  case object Negation extends UnaryOp
  case object Len extends UnaryOp
  case object Ord extends UnaryOp
  case object Chr extends UnaryOp

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
  case class NewPair(fst: Expression, snd: Expression)(val pos: (Int, Int))
      extends RValue
  case class ArrayLiteral(expressions: List[Expression])(val pos: (Int, Int))
      extends RValue
  case class FunctionCall(identifier: Identifier, args: List[Expression])(
      val pos: (Int, Int))
      extends RValue

  sealed trait Expression extends RValue
  case class IntLiteral(value: Int)(val pos: (Int, Int)) extends Expression
  case class BoolLiteral(value: Boolean)(val pos: (Int, Int)) extends Expression
  case class CharLiteral(value: Char)(val pos: (Int, Int)) extends Expression
  case class StringLiteral(value: String)(val pos: (Int, Int))
      extends Expression
  case class PairLiteral(pos: (Int, Int)) extends Expression
  case class UnaryOpApp(op: UnaryOp, expr: Expression)(val pos: (Int, Int))
      extends Expression
  case class BinaryOpApp(op: BinaryOp, lhs: Expression, rhs: Expression)(
      val pos: (Int, Int))
      extends Expression

  trait ParserSingletonBridgePos[+A] {
    def con(pos: (Int, Int)): A
    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
  }

  trait ParserBridgePos0[+A] extends ParserSingletonBridgePos[A] { this: A =>
    def con(pos: (Int, Int)): A = this
  }

  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C]
      extends ParserSingletonBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped(this.apply(_, _) _)
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }

  trait ParserBridgePos3[-A, -B, -C, +D]
      extends ParserSingletonBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C) => D =
      this.apply(_, _, _)(pos)
  }

  // Bridges
  object Program extends ParserBridgePos3[List[Import], List[Func], List[Statement], Program]
  object Import extends ParserBridgePos1[String, Import]
  object Func
      extends ParserBridgePos3[IdentBinding,
                               List[Parameter],
                               List[Statement],
                               Func] {
    override def apply(x1: Parsley[IdentBinding],
                       x2: => Parsley[List[Parameter]],
                       x3: => Parsley[List[Statement]]): Parsley[Func] = {
      def noReturnStatementAtEndOfPath(statements: List[Statement]): Boolean = {
        statements.last match {
          case (IfStatement(_, s1, s2)) =>
            (noReturnStatementAtEndOfPath(s1) || noReturnStatementAtEndOfPath(
              s2))
          case (BeginStatement(s)) => noReturnStatementAtEndOfPath(s)
          case ReturnStatement(_)  => false
          case ExitStatement(_)    => false
          case default             => true
        }
      }
      super.apply(x1, x2, x3).guardAgainst {
        case Func(x1, x2, x3) if x1.typeName == NoneType =>
          Seq(s"Function `" + x1.identifier.name + "` missing type")
        case Func(x1, x2, x3) if x3.isEmpty =>
          Seq(s"Function `" + x1.identifier.name + "` has no statements.")
        case Func(x1, x2, x3) if noReturnStatementAtEndOfPath(x3) =>
          Seq(
            s"Function `" + x1.identifier.name + "` does not have all code paths ending with return or exit statement.")
      }
    }
  }
  object Parameter extends ParserBridgePos2[Type, Identifier, Parameter]

  case object SkipStatement extends Statement with ParserBridge0[Statement]
  object DeclarationStatement
      extends Statement
      with ParserBridgePos3[Type, Identifier, RValue, DeclarationStatement]
  object AssignmentStatement
      extends Statement
      with ParserBridgePos2[LValue, RValue, AssignmentStatement]
  object ReadStatement
      extends Statement
      with ParserBridgePos1[LValue, ReadStatement]
  object FreeStatement
      extends Statement
      with ParserBridgePos1[Expression, FreeStatement]
  object ReturnStatement
      extends Statement
      with ParserBridgePos1[Expression, ReturnStatement]
  object ExitStatement
      extends Statement
      with ParserBridgePos1[Expression, ExitStatement]
  object PrintStatement
      extends Statement
      with ParserBridgePos1[Expression, PrintStatement]
  object PrintLnStatement
      extends Statement
      with ParserBridgePos1[Expression, PrintLnStatement]
  object IfStatement
      extends Statement
      with ParserBridgePos3[Expression,
                            List[Statement],
                            List[Statement],
                            IfStatement]
  object WhileStatement
      extends Statement
      with ParserBridgePos2[Expression, List[Statement], WhileStatement]
  object BeginStatement
      extends Statement
      with ParserBridgePos1[List[Statement], Statement]

  object Identifier
      extends LValue
      with Expression
      with ParserBridgePos1[String, Identifier]
  object IdentBinding extends ParserBridgePos2[Type, Identifier, IdentBinding]
  object PairElem
      extends LValue
      with RValue
      with ParserBridgePos2[PairIndex, LValue, LValue with RValue]
  object ArrayElem
      extends LValue
      with Expression
      with ParserBridgePos2[Identifier,
                            List[Expression],
                            LValue with Expression]
  object IdentOrArrayElem
      extends ParserBridgePos2[String, List[Expression], LValue with Expression] {
    def apply(ident: String, exprs: List[Expression])(pos: (Int, Int)) =
      exprs match {
        case Nil => Identifier(ident)(pos)
        case _   => ArrayElem(Identifier(ident)(pos), exprs)(pos)
      }
  }

  object ArrayType extends Type with ParserBridgePos1[Type, ArrayType] {
    override def apply(t: Type)(pos: (Int, Int)): ArrayType = t match {
      case ArrayType(t, i) => ArrayType(t, i + 1)(pos)
      case _               => ArrayType(t, 1)(pos)
    }
  }
  object PairType
      extends Type
      with ParserBridgePos2[PairElemType, PairElemType, PairType]

  object NewPair
      extends RValue
      with ParserBridgePos2[Expression, Expression, RValue]
  object ArrayLiteral
      extends RValue
      with ParserBridgePos1[List[Expression], RValue]
  object FunctionCall
      extends RValue
      with ParserBridgePos2[Identifier, List[Expression], RValue]

  object IntLiteral extends Expression with ParserBridgePos1[Int, IntLiteral]
  object BoolLiteral
      extends Expression
      with ParserBridgePos1[Boolean, BoolLiteral]
  object CharLiteral extends Expression with ParserBridgePos1[Char, CharLiteral]
  object StringLiteral
      extends Expression
      with ParserBridgePos1[String, StringLiteral]
  case object PairLiteral extends Expression with ParserBridgePos0[Expression]
}
