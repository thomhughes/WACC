package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AST._
import wacc.Parser.parseStatement
import org.scalatest.matchers.should.Matchers._

class ParseStatementUnitTests extends AnyFlatSpec {
    "Parser" should "correctly parse statements with nested pair types" in {
        parseStatement("pair(pair(int, bool), int) x = newpair(q, 4)") should matchPattern {
            case Success(DeclarationStatement(PairType(PairType(IntType, BoolType), IntType), Identifier("x"), NewPair(Identifier("q"), IntLiteral(4)))) =>
        }
    }
}