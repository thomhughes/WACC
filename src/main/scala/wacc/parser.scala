package wacc

import parsley.Parsley

object parser {
  import parsley.combinator.many
  import parsley.Parsley.attempt
  import parsley.Parsley.pure
  import parsley.expr.chain
  import ast._
  import lexer._
  import wacc.lexer.implicits._

  // private lazy val `<program>` = Program("begin" *> many(`<func>`), `<statement>` <* "end")
  // private lazy val `<func>` = Func(`<type>`, `<identifier>`, `<parameters>`, many(`<statement>`))
  // private lazy val `<parameters>` = enclosing.brackets(separators.commaSep(`<parameter>`))
  // private lazy val `<parameter>` = Parameter(`<type>`, `<identifier>`)
  // // TODO: may need to backtrack/consider cases with variable names that conflict with syntax
  // private lazy val `<statement>` = {
  //   DeclarationStatement(`<type>`, `<identifier>`, "=" *> `<rvalue>`) <|>
  //   AssignmentStatement(`<lvalue>`, "=" *> `<rvalue`>) <|> "read" #> ReadStatement(`<lvalue>`) <|>
  //   "free" *> FreeStatement(`<expression>`) <|>
  //   "return" *> ReturnStatement(`<expression>`) <|>
  //   "exit" *> ExitStatement(`<expression>`) <|>
  //   // TODO: need to account for println and print conflict
  //   "print" *> PrintStatement(`<expression>`) <|>
  //   "println" *> PrintLnStatement(`<expression>`) <|>
  //   IfStatement("if" *> `<expression>`, "then" *> many(`<statement>`), "else" *> many(`<statement>`) <* "fi") <|>
  //   WhileStatement("while" *> `<expression>`, "do" *> many(`<statment>`) <* "done") <|>
  //   BeginStatement("begin" *> many(`<statement>`) <* "end")
  // }
  // private lazy val `<rvalue>` = {
  //   `<expression>` <|> 
  //   ArrayLiteral(many(`<expression>`)) <|>
  //   NewPair("newpair" , ) <|>
  //   FunctionCall("call" *> `<identifier>`, '(' *> many(`<expression>`) <* ')')
  // }
  // private lazy val `<expression>` = {
  //   IntLiteral(number)
  // <|> BoolLiteral(("true" #> true) <|> "false" #> false) 
  
  // }

  // lazy val `<type>` = (`<base-type>` <|> `<pair-type>`) <**> (("[]" #> ((x: Type) => ArrayType(x))) <|> pure(identity[Type] _)) 
  lazy val `<type>` = chain.postfix((`<base-type>` <|> `<pair-type>`), ArrayType <# "[" *> "]")
  lazy val `<pair-type>` = "pair" *> (enclosing.parens(PairType(`<pair-elem-type>` <* ",", `<pair-elem-type>`)))
  lazy val `<pair-elem-type>` = (chain.postfix(`<base-type>`, ArrayType <# "[" *> "]")) <|> ("pair" #> PairRefType)
  lazy val `<base-type>` = ("int" #> IntType) <|> ("bool" #> BoolType) <|> ("char" #> CharType) <|> ("string" #> StringType)
}
