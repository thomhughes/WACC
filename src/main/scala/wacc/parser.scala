package wacc

import parsley.Parsley

object parser {
  import parsley.combinator.{many, some}
  import parsley.character._
  import parsley.Parsley.{attempt, pure}
  import parsley.expr.{chain, precedence, Ops, InfixL}
  import parsley.Result
  import ast._
  import lexer._
  import wacc.lexer.implicits._
  import parsley.debug._

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

  private lazy val `<array-elem>` = ArrayElem(Identifier(`<identifier>`), some(enclosing.brackets(`<expression>`)))
  
  lazy val `<base-expression>`: Parsley[Expression] = {
    IntLiteral(number) <|>
    BoolLiteral(("true" #> true) <|> "false" #> false) <|>
    "null" #> PairLiteral <|>
    StringLiteral(`<string>`) <|>
    CharLiteral(ascii) <|> 
    UnaryOpApp(`<unary-op>`, `<expression>`) <|>
    // TODO: not ideal?
    attempt(`<array-elem>`) <|>
    Identifier(`<identifier>`)
  }

  lazy val `<unary-op>`: Parsley[UnaryOp] = {
    "!" #> Not <|>
    "-" #> Negation <|>
    "len" #> Len <|>
    "ord" #> Ord <|>
    "chr" #> Chr
  }

  private def binopParser(opString: String, binOp: BinaryOp) =
    opString #> ((x:Expression, y:Expression) => BinaryOpApp(binOp, x, y))

  lazy val `<expression>`: Parsley[Expression] = precedence[Expression]("(" *> `<expression>` <* ")", `<base-expression>`)(
    Ops(InfixL)(binopParser("*", Mul), binopParser("/", Div), binopParser("%", Mod)),
    Ops(InfixL)(binopParser("+", Plus),binopParser("-", Minus)),
    Ops(InfixL)(
      binopParser("<", Lt), binopParser("<=", Le), binopParser(">", Gt),
      binopParser(">=", Ge), binopParser("!=", Neq), binopParser("==", Eq)
    ),
    Ops(InfixL)(binopParser("&&", And)),
    Ops(InfixL)(binopParser("||", Or))
  )
  

  /*
  private lazy val `<base-type>`: Parsley[BaseType] = {
    ("int" #> IntType) <|> 
    ("bool" #> BoolType) <|> 
    ("char" #> CharType) <|> 
    ("string" #> StringType)
  }
  */


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

  def parse(input: String): Result[String, Expression] =
    `<expression>`.parse(input)
}
