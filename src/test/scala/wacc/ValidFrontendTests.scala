package wacc

class ValidFrontendTests extends IntegrationTest {
    runTestOnDirectory("valid/basic", true, runFrontendTest)
    runTestOnDirectory("valid/advanced", true, runFrontendTest)
    runTestOnDirectory("valid/array", true, runFrontendTest)
    runTestOnDirectory("valid/function", true, runFrontendTest)
    runTestOnDirectory("valid/pairs", true, runFrontendTest)
    runTestOnDirectory("valid/runtimeErr", true, runFrontendTest)
    runTestOnDirectory("valid/if", true, runFrontendTest)
    runTestOnDirectory("valid/variables", true, runFrontendTest)
    runTestOnDirectory("valid/sequence", true, runFrontendTest)
    runTestOnDirectory("valid/expressions", true, runFrontendTest)
    runTestOnDirectory("valid/while", true, runFrontendTest)
    runTestOnDirectory("valid/IO", true, runFrontendTest)
    runTestOnDirectory("valid/scope", true, runFrontendTest)
}