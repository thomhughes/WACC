package wacc

import parsley.Parsley

object parser {
  import parsley.combinator.many
  import parsley.implicits.character.stringLift
  import parsley.combinator.attempt
  import ast._
  import lexer._

  private lazy val `<program>` = Program("begin" *> many(`<func>`), `<statement>` <* "end")
  private lazy val `<func>` = Func(`<type>`, identifier, `<parameters>`, many(`<statement>`))
  private lazy val `<parameters>` = enclosing.brackets(separators.commaSep(`<parameter>`))
  private lazy val `<parameter>` = Parameter(`<type>`, identifier)
  // TODO: may need to backtrack/consider cases with variable names that conflict with syntax
  private lazy val `<statement>` = {
    DeclarationStatement(`<type>`, `<identifier>`, "=" *> `<rvalue>`) <|>
    AssignmentStatement(`<lvalue>`, "=" *> `<rvalue`>) <|> "read" #> ReadStatement(`<lvalue>`) <|>
    "free" *> FreeStatement(`<expression>`) <|>
    "return" *> ReturnStatement(`<expression>`) <|>
    "exit" *> ExitStatement(`<expression>`) <|>
    // TODO: need to account for println and print conflict
    "print" *> PrintStatement(`<expression>`) <|>
    "println" *> PrintLnStatement(`<expression>`) <|>
    IfStatement("if" *> `<expression>`, "then" *> many(`<statement>`), "else" *> many(`<statement>`) <* "fi") <|>
    WhileStatement("while" *> `<expression>`, "do" *> many(`<statment>`) <* "done") <|>
    BeginStatement("begin" *> many(`<statement>`) <* "end")
  }
  private lazy val `<rvalue>` = {
    `<expression>` <|> 
    ArrayLiteral(many(`<expression>`)) <|>
    NewPair("newpair" , ) <|>
    FunctionCall("call" *> `<identifier>`, '(' *> many(`<expression>`) <* ')')
  }
  private lazy val `<expression>` = {
    IntLiteral(number)
  <|> BoolLiteral(("true" #> true) <|> "false" #> false) 
  
  }

  private lazy val `<type>` = ("int" #> IntType) <|> ("bool" #> BoolType) <|> ("char" #> CharType) <|> ("string" #> StringType)

}
