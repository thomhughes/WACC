object lexer {
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
          identifierLetter = predicate.Basic(_.isLetterOrDigit),
          operatorLetter = predicate.Basic(x => false),
          operatorStart = predicate.Basic(x => false)
        ),
      symbolDesc = SymbolDesc.plain.copy(
          hardKeywords = Set("begin", "end", "is", "skip", "read", "free", "return", "exit", "print", "println", "if", "then", "else", "fi", 
            "while", "do", "done", "fst", "snd", "newpair", "call", "int", "bool", "char", "string", "pair", "len", "ord", "chr", "true", "false",
            "null"),
          hardOperators = Set("=", "!", "-", "*", "/", "%", "+", "-", ">", ">=", "<=", "==", "!=", "&&", "||") // might be pessimistic
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
//      textDesc = TextDesc.plain.copy(
//         escapeSequences = 
//       ),
      spaceDesc = SpaceDesc.plain.copy(
          commentStart = "",
          commentEnd = "",
          commentLine = "#",
          commentLineAllowsEOF = true,
          nestedComments = false
        )
    )
}
