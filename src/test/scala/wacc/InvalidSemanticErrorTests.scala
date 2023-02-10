package wacc

class InvalidSemanticErrorTests extends IntegrationTest {
    runTestOnDirectory("invalid/semanticErr/array", true)
    runTestOnDirectory("invalid/semanticErr/function", true)
    runTestOnDirectory("invalid/semanticErr/multiple", true)
    runTestOnDirectory("invalid/semanticErr/pairs", true)
    runTestOnDirectory("invalid/semanticErr/read", true)
    runTestOnDirectory("invalid/semanticErr/scope", true)
    runTestOnDirectory("invalid/semanticErr/exit", true)
    runTestOnDirectory("invalid/semanticErr/if", true)
    runTestOnDirectory("invalid/semanticErr/print", true) 
    runTestOnDirectory("invalid/semanticErr/variables", true) 
    runTestOnDirectory("invalid/semanticErr/expressions", true) 
    runTestOnDirectory("invalid/semanticErr/while", true)
    runTestOnDirectory("invalid/semanticErr/IO", true)
}
