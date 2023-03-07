package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST._
import wacc.Errors._
import wacc.Analyser.checkProgram
import org.scalatest.matchers.should.Matchers._

class SemanticAnalysisUnitTests extends AnyFlatSpec {
    "Semantic analyser" should "correctly work with nested pair types" in {
        checkProgram(Program(List(), List(
            DeclarationStatement(PairType(IntType, BoolType)(0, 0), Identifier("q")(0, 0), NewPair(IntLiteral(3)(0, 0), BoolLiteral(true)(0, 0))(0, 0))(0, 0),
            DeclarationStatement(PairType(PairType(IntType, BoolType)(0, 0), IntType)(0, 0), Identifier("x")(0, 0), NewPair(Identifier("q")(0, 0), IntLiteral(4)(0, 0))(0, 0))(0, 0)
        ))(0, 0)) should matchPattern {
            case (List(), _, _) =>
        }
    }

    "Semantic analyser" should "correctly reject incorrect nested types" in {
        checkProgram(Program(List(), List(
            DeclarationStatement(PairType(IntType, IntType)(0, 0), Identifier("q")(0, 0), NewPair(IntLiteral(3)(0, 0), IntLiteral(5)(0, 0))(0, 0))(0, 0),
            DeclarationStatement(PairType(PairType(IntType, BoolType)(0, 0), IntType)(0, 0), Identifier("x")(0, 0), NewPair(Identifier("q")(0, 0), IntLiteral(4)(0, 0))(0, 0))(0, 0)
        ))(0, 0)) should matchPattern {
            case (TypeError(_, _, _) :: Nil, _, _) =>
        }
    }
}
