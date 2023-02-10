package wacc
import parsley.Parsley
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.errors.LabelConfig
import parsley.token.errors.LabelWithExplainConfig

object Lexer {
  import parsley.token.{Lexer, predicate}
  import parsley.token.descriptions.{
    LexicalDesc,
    NameDesc,
    SymbolDesc,
    SpaceDesc
  }
  import parsley.token.descriptions.numeric.NumericDesc
  import parsley.token.descriptions.numeric.BreakCharDesc.NoBreakChar
  import parsley.token.descriptions.numeric.PlusSignPresence.Optional
  import parsley.token.descriptions.numeric.ExponentDesc.NoExponents
  import parsley.token.descriptions.text.TextDesc
  import parsley.token.errors.Label
  import parsley.token.errors.ErrorConfig

  object errConfig extends ErrorConfig {
    override def labelSymbolOpenSquare: LabelConfig =
      Label("index (`identifier[index]`)")
    override def labelSymbolOpenParen: LabelConfig = Label("open parenthesis")
    override def labelStringAscii(multi: Boolean,
                                  raw: Boolean): LabelWithExplainConfig =
      Label("string literal")
    override def labelCharAscii: LabelWithExplainConfig = Label("char literal")
  }
  import wacc.Keywords.keywords

  val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(x => x.isLetter || x == '_'),
      identifierLetter = predicate.Basic(x => x.isLetterOrDigit || x == '_'),
      operatorLetter = predicate.Basic(x => false)
      // operatorStart = predicate.Basic(x => false)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = keywords,
      hardOperators = Set("=",
                          "!",
                          "-",
                          "*",
                          "/",
                          "%",
                          "+",
                          "-",
                          ">",
                          ">=",
                          "<=",
                          "<",
                          "==",
                          "!=",
                          "&&",
                          "||") // might be pessimistic
    ),
    numericDesc = NumericDesc.plain.copy(
      literalBreakChar = NoBreakChar,
      leadingDotAllowed = false,
      trailingDotAllowed = false,
      leadingZerosAllowed = true,
      positiveSign = Optional,
      integerNumbersCanBeHexadecimal = false,
      integerNumbersCanBeOctal = false,
      realNumbersCanBeOctal = false,
      realNumbersCanBeBinary = false,
      hexadecimalLeads = Set(),
      octalLeads = Set(),
      binaryLeads = Set(),
      decimalExponentDesc = NoExponents,
      hexadecimalExponentDesc = NoExponents,
      octalExponentDesc = NoExponents,
      binaryExponentDesc = NoExponents
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = Set('\\'),
        singleMap = Map(('n', 0x0a),
                        ('0', 0x00),
                        ('b', 0x08),
                        ('t', 0x09),
                        ('f', 0x0c),
                        ('r', 0x0d),
                        ('\"', 0x22),
                        ('\'', 0x27))
      ),
      graphicCharacter = predicate.Basic((x: Char) =>
        x >= ' '.toInt && x != '\"'.toInt && x != '\''.toInt)
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentStart = "",
      commentEnd = "",
      commentLine = "#",
      commentLineAllowsEOF = true,
      nestedComments = false
    )
  )

  val lexer = new Lexer(desc, errConfig)

  val `<identifier>` = lexer.lexeme.names.identifier
  val number = lexer.lexeme.numeric.integer.decimal32
  val `<string>` = lexer.lexeme.text.string.ascii
  val `<character>` = lexer.lexeme.text.character.ascii

  def fully[A](p: Parsley[A]) = lexer.fully(p)
  val implicits = lexer.lexeme.symbol.implicits
  val enclosing = lexer.lexeme.enclosing
  val separators = lexer.lexeme.separators
}
