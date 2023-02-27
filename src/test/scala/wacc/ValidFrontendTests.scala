package wacc

class ValidFrontendTests extends IntegrationTest {
    runTestOnDirectory("valid/basic/skip", true, runFrontendTest)
    runTestOnDirectory("valid/advanced", false, runFrontendTest)
    runTestOnDirectory("valid/array", false, runFrontendTest)
    runTestOnDirectory("valid/function", false, runFrontendTest)
    runTestOnDirectory("valid/pairs", false, runFrontendTest)
    runTestOnDirectory("valid/runtimeErr", false, runFrontendTest)
    runTestOnDirectory("valid/if", false, runFrontendTest)
    runTestOnDirectory("valid/variables", false, runFrontendTest)
    runTestOnDirectory("valid/sequence", false, runFrontendTest)
    runTestOnDirectory("valid/expressions", false, runFrontendTest)
    runTestOnDirectory("valid/while", false, runFrontendTest)
    runTestOnDirectory("valid/IO", false, runFrontendTest)
    runTestOnDirectory("valid/scope", false, runFrontendTest)
}