package wacc

class InvalidSyntaxErrorTests extends IntegrationTest {
    runTestOnDirectory("invalid/syntaxErr/array", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/basic", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/function", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/expressions", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/if", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/literals", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/pairs", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/print", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/sequence", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/variables", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/while", true, runFrontendTest)
    runTestOnDirectory("invalid/syntaxErr/fullpairtypes", true, runFrontendTest)
}