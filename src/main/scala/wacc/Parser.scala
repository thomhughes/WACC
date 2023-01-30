package wacc

import parsley.Parsley
import parsley.implicits.character.stringLift
import wacc.AST._
import parsley.Result

object Parser {
    import wacc.Lexer.fully

    lazy val parseStatement: Parsley[Statement] =
        "skip" #> SkipStatement
    
    def parse(input: String): Result[String, Program] = {
        // lazy val parser: Parsley[Program] = (x => Program(List(), x)) <#> fully("begin" ~> parseStatement <~ "end")
        lazy val parser: Parsley[Program] = ("begin" ~> foo <~ "end")
        lazy val foo: Parsley[Program] = (parseStatement).map(x => Program(List(), x))
        parser.parse(input)
    }




    // lazy val expr: Parsley[Int] = precedence[Int]('(' *> expr <* ')', number)(
    // Ops(InfixL)('*' #> (_ * _)),
    // Ops(InfixL)('+' #> (_ + _), '-' #> (_ - _)))
}