package wacc
import parsley.Parsley
import parsley.token.descriptions.text.EscapeDesc

object Lexer {
  import parsley.token.{Lexer, predicate}
  import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc, SpaceDesc}
  import parsley.token.descriptions.numeric.NumericDesc
  import parsley.token.descriptions.numeric.BreakCharDesc.NoBreakChar
  import parsley.token.descriptions.numeric.PlusSignPresence.Optional
  import parsley.token.descriptions.numeric.ExponentDesc.NoExponents
  import parsley.token.descriptions.text.TextDesc

  val desc = LexicalDesc.plain.copy(
      nameDesc = NameDesc.plain.copy(
          identifierStart = predicate.Basic(x => x.isLetter || x == '_'),
          identifierLetter = predicate.Basic(x => x.isLetterOrDigit || x == '_'),
          operatorLetter = predicate.Basic(x => false)
          // operatorStart = predicate.Basic(x => false)
        ),
      symbolDesc = SymbolDesc.plain.copy(
          hardKeywords = Set("begin", "end", "is", "skip", "read", "free", "return", "exit", "print", "println", "if", "then", "else", "fi", 
            "while", "do", "done", "fst", "snd", "newpair", "call", "int", "bool", "char", "string", "pair", "len", "ord", "chr", "true", "false",
            "null"),
          hardOperators = Set("=", "!", "-", "*", "/", "%", "+", "-", ">", ">=", "<=", "<", "==", "!=", "&&", "||") // might be pessimistic
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
      // TODO: Look at text descriptions.
      textDesc = TextDesc.plain.copy(
        escapeSequences = EscapeDesc.plain.copy(
          escBegin = '\\',
          literals = Set('\\'),
          singleMap = Map(('n', 0x0a), ('0', 0x00), ('b', 0x08), ('t', 0x09), ('f', 0x0c), ('r', 0x0d), ('\"', 0x22), ('\'', 0x27))
        )
      ),
      spaceDesc = SpaceDesc.plain.copy(
          commentStart = "",
          commentEnd = "",
          commentLine = "#",
          commentLineAllowsEOF = true,
          nestedComments = false
        )
    )

    val lexer = new Lexer(desc)

    val `<identifier>` = lexer.lexeme.names.identifier
    val number = lexer.lexeme.numeric.integer.decimal32
    val `<string>` = lexer.lexeme.text.string.ascii
    val char = lexer.lexeme.text.character.fullUtf16
    val ascii = lexer.lexeme.text.character.ascii

    def fully[A](p: Parsley[A]) = lexer.fully(p)
    val implicits = lexer.lexeme.symbol.implicits
    val enclosing = lexer.lexeme.enclosing
    val separators = lexer.lexeme.separators
}
