package wacc

import parsley.{Parsley, Success, Failure}
import parsley.character.{digit, whitespaces}
import parsley.expr.chain
import parsley.implicits.character.charLift
import parsley.combinator.eof
// import wacc.io.readFile
import wacc.parser.parse

/*
object Lexer {
    private val ws = whitespaces

    private def lexeme[A](t: Parsley[A]) = t <~ ws
    def fully[A](t: Parsley[A]): Parsley[A] = ws ~> t <~ eof

    lazy val integer = lexeme {
        digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)
    }
}
*/

object Main {
    // import Lexer._

    def main(args: Array[String]): Unit = {
        println("Hello WACC_42!")

        parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
    }
}
