package wacc

class ValidTests extends IntegrationTest {
    runTestOnDirectory("valid/basic", true)
    runTestOnDirectory("valid/advanced", true)
    runTestOnDirectory("valid/array", true)
    runTestOnDirectory("valid/function", true)
    runTestOnDirectory("valid/pairs", true)
    runTestOnDirectory("valid/runtimeErr", true)
    runTestOnDirectory("valid/if", true)
    runTestOnDirectory("valid/basic", true) 
    runTestOnDirectory("valid/variables", true)
    runTestOnDirectory("valid/sequence", true)
    runTestOnDirectory("valid/expressions", true)
    runTestOnDirectory("valid/while", true)
    runTestOnDirectory("valid/IO", true)
    runTestOnDirectory("valid/scope", true)
}
