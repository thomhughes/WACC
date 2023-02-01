package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Success, Failure}
import wacc.ast._
import wacc.parser.parse
import org.scalatest.matchers.should.Matchers._

class ParserTest extends AnyFlatSpec {

    "Parser" should "correctly parse a basic sum" in {
        parse("3 + 4") should be (Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic subtraction" in {
        parse("5 - 2") should be (Success(BinaryOpApp(Minus, IntLiteral(5), IntLiteral(2))))
    }

    "Parser" should "be able to handle whitespaces in basic sums" in {
        parse("    3   +  4   ") should be (Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic multiplication" in {
        parse("3 * 4") should be (Success(BinaryOpApp(Mul, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse a basic division" in {
        parse("3 / 4") should be (Success(BinaryOpApp(Div, IntLiteral(3), IntLiteral(4))))
    }

    "Parser" should "correctly parse the negation of a variable" in {
        parse("!x") should be (Success(UnaryOpApp(Not, Identifier("x"))))
    }

    "Parser" should "correctly parse nested additions" in {
        parse("3 + (4 + 5)") should be (Success(BinaryOpApp(Plus, IntLiteral(3), BinaryOpApp(Plus, IntLiteral(4), IntLiteral(5)))))
    }

    "Parser" should "correctly follow associativity rules for subtraction" in {
        parse("3 - 4 - 5") should be (Success(BinaryOpApp(Minus, BinaryOpApp(Minus, IntLiteral(3), IntLiteral(4)), IntLiteral(5))))
    }

    "Parser" should "correctly follow precedence rules for multiplication and addition" in {
        parse("3 + 4 * 5") should be (Success(BinaryOpApp(Plus, IntLiteral(3), BinaryOpApp(Mul, IntLiteral(4), IntLiteral(5)))))
    }
    
}