package wacc

import parsley.Parsley
import parsley.errors.DefaultErrorBuilder
import scala.util.Try

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
  import parsley.errors.ErrorBuilder
  import parsley.errors.patterns.VerifiedErrors
  import parsley.errors.combinator._
  import parsley.errors.tokenextractors._
  import parsley.io._

  import scala.io.Codec
  import java.io.File

  // TODO: rewrite attempt
  lazy val `<program>` = Program("begin" *> many(`<func>`), `<statements>` <* "end")
  lazy val `<func>` = Func(attempt(IdentBinding(`<type>`, Identifier(`<identifier>`)) <* "("), `<param-list>` <~ ")", "is" *> `<statements>` <* "end")
  lazy val `<param-list>` = separators.commaSep(`<param>`)
  lazy val `<param>` = Parameter(`<type>`, Identifier(`<identifier>`))

  lazy val `<base-expression>`: Parsley[Expression] = {
    IntLiteral(number).label("int literal") <|>
    BoolLiteral(("true" #> true) <|> "false" #> false).label("bool literal") <|>
    "null" #> PairLiteral <|>
    UnaryOpApp(`<unary-op>`, `<expression>`) <|>
    IdentOrArrayElem(`<identifier>`, many(enclosing.brackets(`<expression>`))) <|>
    CharLiteral(`<character>`) <|>
    StringLiteral(`<string>`)
  }

  lazy val `<unary-op>`: Parsley[UnaryOp] = choice(
    Not <# "!",
    Negation <# "-",
    Len <# "len",
    Ord <# "ord",
    Chr <# "chr",
  ).label("unary operation (not, negation, len, ord, chr)")

  private def _invalidFunctionCall = "()" *> unexpected("function call")
  .explain("function calls need to be prefixed with call and can't be used as an operand in an expression")
  private def binopParser(opString: String, binOp: BinaryOp) =
    (opString #> ((x:Expression, y:Expression) => BinaryOpApp(binOp, x, y))).label("binary operation")// <|>
    // amend(_invalidFunctionCall)

  lazy val `<expression>`: Parsley[Expression] = precedence[Expression](enclosing.parens(`<expression>`), `<base-expression>`)(
    Ops(InfixL)(binopParser("*", Mul), binopParser("/", Div), binopParser("%", Mod)),
    Ops(InfixL)(binopParser("+", Plus), binopParser("-", Minus)),
    Ops(InfixL)(
      binopParser("<", Lt), binopParser("<=", Le), binopParser(">", Gt),
      binopParser(">=", Ge), binopParser("!=", Neq), binopParser("==", Eq)
    ),
    Ops(InfixL)(binopParser("&&", And)),
    Ops(InfixL)(binopParser("||", Or))
  )

  lazy val `<array-type>` = (ArrayType <# "[" *> "]").label("[] (array type)")
  lazy val `<type>` = chain.postfix((`<base-type>` <|> `<pair-type>`), `<array-type>`).label("type")
  lazy val `<pair-type>` = "pair" *> (enclosing.parens(PairType(`<pair-elem-type>` <* ",", `<pair-elem-type>`))).label("pair type")
  lazy val `<pair-elem-type>` = (chain.postfix(`<base-type>`, `<array-type>`)) <|> (PairRefType <# "pair").label("pair element type")
  lazy val `<base-type>` = (IntType <# "int") <|> (BoolType <# "bool") <|> (CharType <# "char") <|> (StringType <# "string")

  // TODO: attempt not ideal?
  lazy val `<lvalue>`: Parsley[LValue] = {
    `<pair-elem>` <|>
    IdentOrArrayElem(`<identifier>`, many(enclosing.brackets(`<expression>`)))
  }.explain("because an lvalue needed")

  lazy val `<pair-elem>` = {
    ("fst" *> PairElem(pure(Fst), `<lvalue>`)) <|>
    ("snd" *> PairElem(pure(Snd), `<lvalue>`))
  }

  lazy val `<rvalue>` = {
    `<expression>` <|>
    ArrayLiteral(enclosing.brackets(separators.commaSep(`<expression>`))) <|>
    NewPair("newpair" *> ("(" *> `<expression>`), "," *> `<expression>` <* ")") <|>
    FunctionCall("call" *> Identifier(`<identifier>`), enclosing.parens(separators.commaSep(`<expression>`))) <|>
    `<pair-elem>`
  }.explain("because an rvalue needed")

  lazy val `<assignment>` = ("=" *> `<rvalue>`).label("assignment") <|> (enclosing.parens(pure(true)) *> "is").hide.verifiedFail("aaa")
  lazy val `<statement>`: Parsley[Statement] = {
    "skip" *> pure(SkipStatement) <|>
    amend(DeclarationStatement(entrench(`<type>`), entrench(Identifier(`<identifier>`)), `<assignment>`)) <|>
    AssignmentStatement(`<lvalue>`, `<assignment>`) <|> 
    "read" *> ReadStatement(`<lvalue>`) <|>
    "free" *> FreeStatement(`<expression>`) <|>
    "return" *> ReturnStatement(`<expression>`) <|>
    "exit" *> ExitStatement(`<expression>`) <|>
    "print" *> PrintStatement(`<expression>`) <|>
    "println" *> PrintLnStatement(`<expression>`) <|>
    IfStatement("if" *> `<expression>`, "then" *> `<statements>`, "else" *> `<statements>` <* "fi") <|>
    WhileStatement("while" *> `<expression>`, "do" *> `<statements>` <* "done") <|>
    BeginStatement("begin" *> `<statements>` <* "end")
    // amend(entrench(`<func>` *> unexpected("function declaration").explain("all functions must be declared at the top of the main block")))
   }.explain("because a statement is required")
  
  lazy val `<statements>` = separators.semiSep(`<statement>`)

  def parseExpression(input: String): Result[String, Expression] =
    fully(`<expression>`).parse(input)

  class NewErrorBuilder extends DefaultErrorBuilder with SingleChar

  //   // override def tokens: Seq[Parsley[String]] = Seq(lexer.nonlexeme.names.identifier.map(s"identifier " + _)) ++ desc.symbolDesc.hardKeywords.toSeq.map(string(_).map(s"keyword" + _))
  //   //++ desc.symbolDesc.hardKeywords.toSeq.map(string(_).map("keyword" + _)) ++ desc.symbolDesc.hardOperators.toSeq.map(string(_).map("keyword" + _))

  //   // override def trimToParserDemand: Boolean = false
  // }

  implicit val errBuilder: NewErrorBuilder = new NewErrorBuilder
  // def parse(input: String): Result[String, Program] = fully(`<program>`).parse(input)

  implicit val codec: Codec = Codec.UTF8
  def parse(input: File): Try[parsley.Result[String, Program]] = (fully(`<program>`)).parseFromFile(input)
}
