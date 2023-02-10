package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Success
import wacc.AST._
import wacc.Parser.parseExpression
import org.scalatest.matchers.should.Matchers._

class ParseExpressionUnitTests extends AnyFlatSpec {

    "Parser" should "correctly parse a basic sum" in {
        parseExpression("3 + 4") should matchPattern { case Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic subtraction" in {
        parseExpression("5 - 2") should matchPattern { case Success(BinaryOpApp(Minus, IntLiteral(5), IntLiteral(2))) => }
    }

    "Parser" should "be able to handle whitespaces in basic sums" in {
        parseExpression("    3   +  4   ") should matchPattern { case Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic multiplication" in {
        parseExpression("3 * 4") should matchPattern { case Success(BinaryOpApp(Mul, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic division" in {
        parseExpression("3 / 4") should matchPattern { case Success(BinaryOpApp(Div, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic modular arithmetic calculation" in {
        parseExpression("3 % 4") should matchPattern { case Success(BinaryOpApp(Mod, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse the negation of a variable" in {
        parseExpression("!x") should matchPattern { case Success(UnaryOpApp(Not, Identifier("x"))) => }
    }

    "Parser" should "correctly parse nested additions" in {
        parseExpression("3 + (4 + 5)") should matchPattern { case Success(BinaryOpApp(Plus, IntLiteral(3), BinaryOpApp(Plus, IntLiteral(4), IntLiteral(5)))) => }
    }

    "Parser" should "correctly follow associativity rules for subtraction" in {
        parseExpression("3 - 4 - 5") should matchPattern { case Success(BinaryOpApp(Minus, BinaryOpApp(Minus, IntLiteral(3), IntLiteral(4)), IntLiteral(5))) => }
    }

    "Parser" should "correctly follow precedence rules for multiplication and addition" in {
        parseExpression("3 + 4 * 5") should matchPattern { case Success(BinaryOpApp(Plus, IntLiteral(3), BinaryOpApp(Mul, IntLiteral(4), IntLiteral(5)))) => }
    }

    "Parser" should "correctly follow precedence rules for division and subtraction" in {
        parseExpression("3 - 4 / 5") should matchPattern { case Success(BinaryOpApp(Minus, IntLiteral(3), BinaryOpApp(Div, IntLiteral(4), IntLiteral(5)))) => }
    }

    "Parser" should "correctly parse a basic 'less than'" in {
        parseExpression("3 < 4") should matchPattern { case Success(BinaryOpApp(Lt, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic equality" in {
        parseExpression("3 == 4") should matchPattern { case Success(BinaryOpApp(Eq, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic inequality" in {
        parseExpression("3 != 4") should matchPattern { case Success(BinaryOpApp(Neq, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic 'greater than'" in {
        parseExpression("3 > 4") should matchPattern { case Success(BinaryOpApp(Gt, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic 'less than or equal to'" in {
        parseExpression("3 <= 4") should matchPattern { case Success(BinaryOpApp(Le, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic 'greater than or equal to'" in {
        parseExpression("3 >= 4") should matchPattern { case Success(BinaryOpApp(Ge, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly complex equality expressions" in {
        parseExpression("3 + 4 == 5") should matchPattern { case Success(BinaryOpApp(Eq, BinaryOpApp(Plus, IntLiteral(3), IntLiteral(4)), IntLiteral(5))) => }
    }

    "Parser" should "correctly parse a basic 'and'" in {
        parseExpression("3 && 4") should matchPattern { case Success(BinaryOpApp(And, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic 'or'" in {
        parseExpression("3 || 4") should matchPattern { case Success(BinaryOpApp(Or, IntLiteral(3), IntLiteral(4))) => }
    }

    "Parser" should "correctly parse a basic 'and' with 'or'" in {
        parseExpression("3 && 4 || 5") should matchPattern { case Success(BinaryOpApp(Or, BinaryOpApp(And, IntLiteral(3), IntLiteral(4)), IntLiteral(5))) => }
    }

    "Parser" should "correctly parse negative numbers" in {
        parseExpression("-3") should matchPattern { case Success(IntLiteral(-3)) => }
    }

    "Parser" should "correctly parse negative numbers in expressions" in {
        parseExpression("3 + -4") should matchPattern { case Success(BinaryOpApp(Plus, IntLiteral(3), IntLiteral(-4))) => }
    }

    "Parser" should "correctly parse the length of an array" in {
        parseExpression("len(arr)") should matchPattern { case Success(UnaryOpApp(Len, Identifier("arr"))) => }
    }

    "Parser" should "correctly parse the length of an array given any input format" in {
        parseExpression("len arr") should matchPattern { case Success(UnaryOpApp(Len, Identifier("arr"))) => }
    }

    "Parser" should "correctly parse the ord of a character" in {
        parseExpression("ord('a')") should matchPattern { case Success(UnaryOpApp(Ord, CharLiteral('a'))) => }
    }

    "Parser" should "correctly parse the chr of an integer" in {
        parseExpression("chr(97)") should matchPattern { case Success(UnaryOpApp(Chr, IntLiteral(97))) => }
    }

    "Parser" should "correctly parse true bool literals" in {
        parseExpression("true") should matchPattern { case Success(BoolLiteral(true)) => }
    }

    "Parser" should "correctly parse false bool literals" in {
        parseExpression("false") should matchPattern { case Success(BoolLiteral(false)) => }
    }

    "Parser" should "correctly parse boolean expressions with 'and' and 'or'" in {
        parseExpression("(true || false) && false") should matchPattern { case Success(BinaryOpApp(And, BinaryOpApp(Or, BoolLiteral(true), BoolLiteral(false)), BoolLiteral(false))) => }
    }

    "Parser" should "correctly follow precedence for 'and' and 'or'" in {
        parseExpression("true && false || true") should matchPattern { case Success(BinaryOpApp(Or, BinaryOpApp(And, BoolLiteral(true), BoolLiteral(false)), BoolLiteral(true))) => }
    }

    "Parser" should "correctly parse character literals" in {
        parseExpression("'a'") should matchPattern { case Success(CharLiteral('a')) => }
    }

    "Parser" should "correctly parse string literals" in {
        parseExpression("\"hello\"") should matchPattern { case Success(StringLiteral("hello")) => }
    }

    "Parser" should "correctly parse string literals with escaped characters" in {
        parseExpression("\"hello\\\"\"") should matchPattern { case Success(StringLiteral("hello\"")) => }
    }

    "Parser" should "correctly parse null pair literals" in {
        parseExpression("null") should matchPattern { case Success(PairLiteral) => }
    }

    "Parser" should "correctly parse identifier" in {
        parseExpression("x") should matchPattern { case Success(Identifier("x")) => }
    }

    "Parser" should "correctly parse identifier with underscore" in {
        parseExpression("x_y") should matchPattern { case Success(Identifier("x_y")) => }
    }

    "Parser" should "correctly parse identifier with numbers" in {
        parseExpression("x1") should matchPattern { case Success(Identifier("x1")) => }
    }

    "Parser" should "correctly parse identifier with numbers and underscore" in {
        parseExpression("x_1") should matchPattern { case Success(Identifier("x_1")) => }
    }

    "Parser" should "correctly parse expressions containing identifiers" in {
        parseExpression("x + 1") should matchPattern { case Success(BinaryOpApp(Plus, Identifier("x"), IntLiteral(1))) => }
    }
}