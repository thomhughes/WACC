package wacc

class InvalidSemanticErrorTests extends IntegrationTest {
    runTestOnDirectory("invalid/semanticErr/array", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/function", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/multiple", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/pairs", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/read", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/scope", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/exit", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/if", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/print", true, runFrontendTest) 
    runTestOnDirectory("invalid/semanticErr/variables", true, runFrontendTest) 
    runTestOnDirectory("invalid/semanticErr/expressions", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/while", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/IO", true, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/fullpairtypes", true, runFrontendTest)
}