package wacc

import parsley.Parsley

object parser {
  import parsley.combinator.many
  import wacc.lexer.implicits._
  import parsley.character._
  import ast._
  import lexer._

  /*
  private lazy val `<program>` = Program("begin" *> many(`<func>`), `<statements>` <* "end")
  private lazy val `<func>` = Func(`<type>`, `<identifier>`, `<param-list>`, "is" *> `<statements>` <* "end")
  private lazy val `<param-list>` = enclosing.parens(separators.commaSep(`<param>`))
  private lazy val `<param>` = Parameter(`<type>`, `<identifier>`)
  */
  // TODO: may need to backtrack/consider cases with variable names that conflict with syntax
  /*
  private lazy val `<statement>`: Parsley[Statement] = {
    "skip" #> SkipStatement <|>
    DeclarationStatement(`<type>`, `<identifier>`, "=" *> `<rvalue>`) <|>
    AssignmentStatement(`<lvalue>`, '=' *> `<rvalue`>) <|> 
    "read" #> ReadStatement(`<lvalue>`) <|>    
    "free" *> FreeStatement(`<expression>`) <|>
    "return" *> ReturnStatement(`<expression>`) <|>
    "exit" *> ExitStatement(`<expression>`) <|>
    // TODO: need to account for println and print conflict
    "print" *> PrintStatement(`<expression>`) <|>
    "println" *> PrintLnStatement(`<expression>`) <|>
    IfStatement("if" *> `<expression>`, "then" *> `<statements>`, "else" *> `<statements>` <* "fi") <|>
    WhileStatement("while" *> `<expression>`, "do" *> `<statments>` <* "done") <|>
    BeginStatement("begin" *> `<statements>` <* "end")
  }
  */
  /*
  private lazy val `<lvalue>` = {
    `<identifier>` <|>
    `<array-elem>` <|>
    `<pair-elem>`
  }
  */
  /*
  private lazy val `<rvalue>` = {
    `<expression>` <|>
    ArrayLiteral(enclosing.brackets(separators.commaSep(`<expression>`))) <|>
    NewPair("newpair" *> ('(' *> `<expression>`), ',' *> `<expression>` <* ')') <|>
    FunctionCall("call" *> `<identifier>`, enclosing.parens(separators.commaSep(`<expression>`))) <|>
    `<pair-elem>`
  }
  */
  /*
  private lazy val `<pair-elem>` = {
    ("fst" *> PairElem(Fst, `<lvalue>`))  <|>
    ("snd" *> PairElem(Snd, `<lvalue>`))
  }
  */
  
  private lazy val `<expression>` = {
    IntLiteral(number) <|>
    BoolLiteral(("true" #> true) <|> "false" #> false) <|>
    CharLiteral(ascii) <|>
    StringLiteral("\"" *> stringOfSome(letter) <* "\"") <|>
    "null" #> PairLiteral
    // `<identifier>` <|>
    // `<array-elem>`
  }
  /*
  private lazy val `<base-type>`: Parsley[BaseType] = {
    ("int" #> IntType) <|> 
    ("bool" #> BoolType) <|> 
    ("char" #> CharType) <|> 
    ("string" #> StringType)
  }
  */

  object implicits {
   
  }
}