package wacc

class InvalidSyntaxErrorTests extends IntegrationTest {
    runTestOnDirectory("invalid/syntaxErr/array", true)
    runTestOnDirectory("invalid/syntaxErr/basic", true)
    runTestOnDirectory("invalid/syntaxErr/function", true)
    runTestOnDirectory("invalid/syntaxErr/expressions", true)
    runTestOnDirectory("invalid/syntaxErr/if", true)
    runTestOnDirectory("invalid/syntaxErr/literals", true)
    runTestOnDirectory("invalid/syntaxErr/pairs", true)
    runTestOnDirectory("invalid/syntaxErr/print", true)
    runTestOnDirectory("invalid/syntaxErr/sequence", true)
    runTestOnDirectory("invalid/syntaxErr/variables", true)
    runTestOnDirectory("invalid/syntaxErr/while", true)
}
