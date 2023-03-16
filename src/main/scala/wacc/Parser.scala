package wacc

import parsley.Parsley
import scala.util.Try
import parsley.expr.Prefix

object Parser {
  import parsley.combinator.many
  import parsley.combinator.choice
  import parsley.Parsley.{attempt, pure}
  import parsley.expr.{chain, precedence, Ops, InfixL}
  import parsley.Result
  import AST._
  import Lexer._
  import wacc.Lexer.implicits._
  import parsley.errors.combinator.ErrorMethods
  import parsley.errors.patterns.VerifiedErrors
  import parsley.errors.combinator._
  import parsley.io._
  import parsley.position.pos

  import wacc.SyntaxErrorBuilder
  import wacc.Errors.SyntaxError

  import scala.io.Codec
  import java.io.File

  lazy val `<program>` =
    Program(many("import" *> `<import>`),
            "begin" *> many(`<func>`),
            `<statements>` <* "end")
  lazy val `<import>` = Import(`<string>`)
  lazy val `<func>` = Func(attempt(
                             IdentBinding(`<type>` <|> pure(NoneType),
                                          Identifier(`<identifier>`)) <* "("),
                           `<param-list>` <~ ")",
                           "is" *> separators.semiSep(`<statement>`) <* "end")
  lazy val `<param-list>` = separators.commaSep(`<param>`)
  lazy val `<param>` = Parameter(`<type>`, Identifier(`<identifier>`))

  def parseAsProgram(input: String): Program = {
    `<program>`.parse(input).get
  }

  def returnStatementAtEndOfAllPaths(statements: List[Statement]): Boolean = {
    statements.last match {
      case (IfStatement(_, s1, s2)) =>
        (returnStatementAtEndOfAllPaths(s1) && returnStatementAtEndOfAllPaths(
          s2))
      case WhileStatement(_, s) => returnStatementAtEndOfAllPaths(s)
      case BeginStatement(s)    => returnStatementAtEndOfAllPaths(s)
      case ReturnStatement(_)   => true
      case ExitStatement(_)     => true
      case default              => false
    }
  }

  lazy val `<base-expression>` : Parsley[Expression] = {
    IntLiteral(number) <|>
      BoolLiteral(("true" #> true) <|> "false" #> false) <|>
      "null" #> PairLiteral <|>
      IdentOrArrayElem(`<identifier>`, many(enclosing.brackets(`<expression>`))) <|>
      CharLiteral(`<character>`) <|>
      StringLiteral(`<string>`)
  }

  private def _invalidFunctionCall =
    "()".hide *> unexpected("function call")
      .explain(
        "function calls need to be prefixed with call and can't be used as an operand in an expression")
  private def binopParser(opString: String, binOp: BinaryOp) =
    (opString #> ((pos: (Int, Int)) =>
      (x: Expression, y: Expression) => BinaryOpApp(binOp, x, y)(pos)))
      .label("binary operation") <|> amend(_invalidFunctionCall)

  private def unaryopParser(opString: String, unaryOp: UnaryOp) =
    (opString #> ((pos: (Int, Int)) =>
      (x: Expression) => UnaryOpApp(unaryOp, x)(pos)))
      .label("unary operation") <|> amend(_invalidFunctionCall)

  lazy val `<unary-operator>` = choice("!" #> Not,
                                       "-" #> Negation,
                                       "len" #> Len,
                                       "ord" #> Ord,
                                       "chr" #> Chr)
  lazy val `<unary-op>` : Parsley[Expression] =
    precedence[Expression](`<base-expression>`, "(" *> `<expression>` <* ")")(
      Ops(Prefix)(
        pos <**> unaryopParser("!", Not),
        pos <**> unaryopParser("-", Negation),
        pos <**> unaryopParser("len", Len),
        pos <**> unaryopParser("ord", Ord),
        pos <**> unaryopParser("chr", Chr)
      )
    )

  lazy val `<expression>` : Parsley[Expression] =
    precedence[Expression](`<base-expression>` <|> `<unary-op>`,
                           "(" *> `<expression>` <* ")")(
      Ops(InfixL)(pos <**> binopParser("*", Mul),
                  pos <**> binopParser("/", Div),
                  pos <**> binopParser("%", Mod)),
      Ops(InfixL)(pos <**> binopParser("+", Plus),
                  pos <**> binopParser("-", Minus)),
      Ops(InfixL)(
        pos <**> binopParser("<", Lt),
        pos <**> binopParser("<=", Le),
        pos <**> binopParser(">", Gt),
        pos <**> binopParser(">=", Ge),
        pos <**> binopParser("!=", Neq),
        pos <**> binopParser("==", Eq)
      ),
      Ops(InfixL)(pos <**> binopParser("&&", And)),
      Ops(InfixL)(pos <**> binopParser("||", Or))
    )

  lazy val `<array-type>` = (ArrayType <# "[" *> "]").label("[] (array type)")
  lazy val `<type>` = chain
    .postfix((`<base-type>` <|> `<pair-type>`), `<array-type>`)
    .label("type")
  lazy val `<pair-type>` : Parsley[PairType] = "pair" *> (enclosing
    .parens(PairType(`<pair-elem-type>` <* ",", `<pair-elem-type>`)))
    .label("pair type")
  lazy val `<pair-elem-type>` = (chain
    .postfix(`<base-type>`, `<array-type>`)) <|> attempt(`<pair-type>`) <|> (PairRefType <# "pair")
    .label("pair element type")
  lazy val `<base-type>` = (IntType <# "int") <|> (BoolType <# "bool") <|> (CharType <# "char") <|> (StringType <# "string")

  lazy val `<lvalue>` : Parsley[LValue] = {
    `<pair-elem>` <|>
      IdentOrArrayElem(`<identifier>`, many(enclosing.brackets(`<expression>`)))
  }

  lazy val `<pair-elem>` = {
    ("fst" *> PairElem(pure(Fst), `<lvalue>`)) <|>
      ("snd" *> PairElem(pure(Snd), `<lvalue>`))
  }

  lazy val `<rvalue>` = {
    `<expression>` <|>
      ArrayLiteral(enclosing.brackets(separators.commaSep(`<expression>`))) <|>
      NewPair("newpair" *> ("(" *> `<expression>`),
              "," *> `<expression>` <* ")") <|>
      FunctionCall("call" *> Identifier(`<identifier>`),
                   enclosing.parens(separators.commaSep(`<expression>`))) <|>
      `<pair-elem>`
  }.explain("because an rvalue needed")

  lazy val `<assignment>` = ("=" *> `<rvalue>`).label("assignment") <|> dislodge(
    enclosing.parens(pure(true)) *> "is").hide.verifiedFail("aaa")
  lazy val `<statement>` : Parsley[Statement] = {
    "skip" *> pure(SkipStatement) <|>
      amend(DeclarationStatement(entrench(`<type>`),
                                 entrench(Identifier(`<identifier>`)),
                                 entrench(`<assignment>`))) <|>
      AssignmentStatement(`<lvalue>`, `<assignment>`) <|>
      "read" *> ReadStatement(`<lvalue>`) <|>
      "free" *> FreeStatement(`<expression>`) <|>
      "return" *> ReturnStatement(`<expression>`) <|>
      "exit" *> ExitStatement(`<expression>`) <|>
      "print" *> PrintStatement(`<expression>`) <|>
      "println" *> PrintLnStatement(`<expression>`) <|>
      IfStatement("if" *> `<expression>`,
                  "then" *> `<statements>`,
                  "else" *> `<statements>` <* "fi") <|>
      WhileStatement("while" *> `<expression>`,
                     "do" *> `<statements>` <* "done") <|>
      BeginStatement("begin" *> `<statements>` <* "end")
  }.explain("because a statement is required")

  lazy val `<statements>` = separators.semiSep1(`<statement>`)

  def parseExpression(input: String): Result[String, Expression] =
    fully(`<expression>`).parse(input)

  def parseStatement(input: String): Result[String, Statement] =
    fully(`<statement>`).parse(input)

  implicit val errBuilder = SyntaxErrorBuilder

  implicit val codec: Codec = Codec.UTF8
  def parse(input: File): Try[parsley.Result[SyntaxError, Program]] =
    (fully(`<program>`)).parseFromFile(input)
}
