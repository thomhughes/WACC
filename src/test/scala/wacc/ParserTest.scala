package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Failure, Success}
import wacc.ast._
import wacc.parser.{parse, parseExpression}
import org.scalatest.matchers.should.Matchers._

class ParserTest extends AnyFlatSpec {

    "Parser" should "correctly parse a basic sum" in {
        parseExpression("3 + 4") should be (Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic subtraction" in {
        parseExpression("5 - 2") should be (Success(BinaryOpApp(Minus, IntLiteral(5), IntLiteral(2))))
    }

    "Parser" should "be able to handle whitespaces in basic sums" in {
        parseExpression("    3   +  4   ") should be (Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic multiplication" in {
        parseExpression("3 * 4") should be (Success(BinaryOpApp(Mul, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic division" in {
        parseExpression("3 / 4") should be (Success(BinaryOpApp(Div, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse the negation of a variable" in {
        parseExpression("!x") should be (Success(UnaryOpApp(Not, Identifier("x"))))
    }

    "Parser" should "correctly parse nested additions" in {
        parseExpression("3 + (4 + 5)") should be (Success(BinaryOpApp(Plus, IntLiteral(3), BinaryOpApp(Plus, IntLiteral(4), IntLiteral(5)))))
    }

    "Parser" should "correctly follow associativity rules for subtraction" in {
        parseExpression("3 - 4 - 5") should be (Success(BinaryOpApp(Minus, BinaryOpApp(Minus, IntLiteral(3), IntLiteral(4)), IntLiteral(5))))
    }

    "Parser" should "correctly follow precedence rules for multiplication and addition" in {
        parseExpression("3 + 4 * 5") should be (Success(BinaryOpApp(Plus, IntLiteral(3), BinaryOpApp(Mul, IntLiteral(4), IntLiteral(5)))))
    }

    "Parser" should "correctly follow precedence rules for division and subtraction" in {
        parseExpression("3 - 4 / 5") should be (Success(BinaryOpApp(Minus, IntLiteral(3), BinaryOpApp(Div, IntLiteral(4), IntLiteral(5)))))
    }

    "Parser" should "correctly parse a basic 'less than'" in {
        parseExpression("3 < 4") should be (Success(BinaryOpApp(Lt, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic equality" in {
        parseExpression("3 == 4") should be (Success(BinaryOpApp(Eq, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic inequality" in {
        parseExpression("3 != 4") should be (Success(BinaryOpApp(Neq, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic 'greater than'" in {
        parseExpression("3 > 4") should be (Success(BinaryOpApp(Gt, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic 'less than or equal to'" in {
        parseExpression("3 <= 4") should be (Success(BinaryOpApp(Le, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic 'greater than or equal to'" in {
        parseExpression("3 >= 4") should be (Success(BinaryOpApp(Ge, IntLiteral(3), IntLiteral(4))))
    }
    
}