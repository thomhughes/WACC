package wacc

import parsley.Parsley
import parsley.expr.Prefix

object Parser {
  import parsley.combinator.many
  import parsley.Parsley.{attempt, pure}
  import parsley.expr.{chain, precedence, Ops, InfixL}
  import parsley.Result
  import AST._
  import Lexer._
  import wacc.Lexer.implicits._
  import parsley.implicits.character.charLift

  // TODO: rewrite attempt
  private lazy val `<program>` = Program("begin" *> many(attempt(`<func>`)), `<statements>` <* "end")
  private lazy val `<func>` = Func(attempt(IdentBinding(`<type>`, Identifier(`<identifier>`)) <* "("), `<param-list>` <~ ")", "is" *> `<statements>`.filter(returnStatementAtEndOfAllPaths) <* "end")
  private lazy val `<param-list>` = separators.commaSep(`<param>`)
  private lazy val `<param>` = Parameter(`<type>`, Identifier(`<identifier>`))

  def returnStatementAtEndOfAllPaths(statements: List[Statement]): Boolean = {
    statements.last match {
      case ReturnStatement(_) => true
      case (IfStatement(_, s1, s2)) => (returnStatementAtEndOfAllPaths(s1) && returnStatementAtEndOfAllPaths(s2))
      case WhileStatement(_, s) => returnStatementAtEndOfAllPaths(s)
      case ExitStatement(_) => true
      case default => false
    }
  }

  private lazy val `<base-expression>`: Parsley[Expression] = {
    IntLiteral(number) <|>
    BoolLiteral(("true" #> true) <|> "false" #> false) <|>
    "null" #> PairLiteral <|>
    IdentOrArrayElem(`<identifier>`, many(enclosing.brackets(`<expression>`))) <|>
    CharLiteral(`<character>`) <|>
    StringLiteral(`<string>`)
  }

  private lazy val `<character>`: Parsley[Char] = {
    attempt("\'\\") *> `<escaped-char>` <* "\'" <|>
    ascii.filterNot((x => x == '\"' || x == '\'' || x == '\\'))
  }

  private lazy val `<escaped-char>` = {
    '0' #> '\u0000' <|>
    'b' #> '\b' <|>
    't' #> '\t' <|>
    'n' #> '\n' <|>
    'f' #> '\f' <|>
    'r' #> '\r' <|>
    '\"' <|>
    '\'' <|>
    '\\'
  }

  private def binopParser(opString: String, binOp: BinaryOp) =
    opString #> ((x:Expression, y:Expression) => BinaryOpApp(binOp, x, y))

  private def unaryopParser(opString: String, unaryOp: UnaryOp) =
    opString #> ((x:Expression) => UnaryOpApp(unaryOp, x))

  private lazy val `<expression>`: Parsley[Expression] = precedence[Expression](`<base-expression>`, "(" *> `<expression>` <* ")")(
    Ops(Prefix)(unaryopParser("!", Not), unaryopParser("-", Negation), unaryopParser("len", Len), 
    unaryopParser("ord", Ord), unaryopParser("chr", Chr)),
    Ops(InfixL)(binopParser("*", Mul), binopParser("/", Div), binopParser("%", Mod)),
    Ops(InfixL)(binopParser("+", Plus),binopParser("-", Minus)),
    Ops(InfixL)(
      binopParser("<", Lt), binopParser("<=", Le), binopParser(">", Gt),
      binopParser(">=", Ge), binopParser("!=", Neq), binopParser("==", Eq)
    ),
    Ops(InfixL)(binopParser("&&", And)),
    Ops(InfixL)(binopParser("||", Or))
  )
  
  private lazy val `<type>` = chain.postfix((`<base-type>` <|> `<pair-type>`), ArrayType <# "[" *> "]")
  private lazy val `<pair-type>` = "pair" *> (enclosing.parens(PairType(`<pair-elem-type>` <* ",", `<pair-elem-type>`)))
  private lazy val `<pair-elem-type>` = (chain.postfix(`<base-type>`, ArrayType <# "[" *> "]")) <|> ("pair" #> PairRefType)
  private lazy val `<base-type>` = ("int" #> IntType) <|> ("bool" #> BoolType) <|> ("char" #> CharType) <|> ("string" #> StringType)

  // TODO: attempt not ideal?
  private lazy val `<lvalue>`: Parsley[LValue] = {
    `<pair-elem>` <|>
    IdentOrArrayElem(`<identifier>`, many(enclosing.brackets(`<expression>`)))
  }

  private lazy val `<pair-elem>` = {
    ("fst" *> PairElem(pure(Fst), `<lvalue>`))  <|>
    ("snd" *> PairElem(pure(Snd), `<lvalue>`))
  }

  private lazy val `<rvalue>` = {
    `<expression>` <|>
    ArrayLiteral(enclosing.brackets(separators.commaSep(`<expression>`))) <|>
    NewPair("newpair" *> ("(" *> `<expression>`), "," *> `<expression>` <* ")") <|>
    FunctionCall("call" *> Identifier(`<identifier>`), enclosing.parens(separators.commaSep(`<expression>`))) <|>
    `<pair-elem>`
  }

  private lazy val `<statement>`: Parsley[Statement] = {
    "skip" *> pure(SkipStatement) <|>
    DeclarationStatement(`<type>`, Identifier(`<identifier>`), "=" *> `<rvalue>`) <|>
    AssignmentStatement(`<lvalue>`, "=" *> `<rvalue>`) <|> 
    "read" *> ReadStatement(`<lvalue>`) <|>
    "free" *> FreeStatement(`<expression>`) <|>
    "return" *> ReturnStatement(`<expression>`) <|>
    "exit" *> ExitStatement(`<expression>`) <|>
    "print" *> PrintStatement(`<expression>`) <|>
    "println" *> PrintLnStatement(`<expression>`) <|>
    IfStatement("if" *> `<expression>`, "then" *> `<statements>`, "else" *> `<statements>` <* "fi") <|>
    WhileStatement("while" *> `<expression>`, "do" *> `<statements>` <* "done") <|>
    BeginStatement("begin" *> `<statements>` <* "end")
  }
  
  private lazy val `<statements>` = separators.semiSep1(`<statement>`)

  def parseExpression(input: String): Result[String, Expression] =
    fully(`<expression>`).parse(input)

  def parse(input: String): Result[String, Program] =
    fully(`<program>`).parse(input)
}
