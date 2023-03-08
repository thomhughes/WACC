package wacc

class InvalidSemanticErrorTests extends IntegrationTest {
    runTestOnDirectory("invalid/semanticErr/array", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/function", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/multiple", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/pairs", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/read", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/scope", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/exit", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/if", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/print", false, runFrontendTest) 
    runTestOnDirectory("invalid/semanticErr/variables", false, runFrontendTest) 
    runTestOnDirectory("invalid/semanticErr/expressions", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/while", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/IO", false, runFrontendTest)
    runTestOnDirectory("invalid/semanticErr/overloading", true, runFrontendTest)
}