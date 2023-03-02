package wacc

class BackendTests extends IntegrationTest {
  runTestOnDirectory("valid/basic", false, runBackendTest)
  runTestOnDirectory("valid/array", false, runBackendTest)
  runTestOnDirectory("valid/function", false, runBackendTest)
  runTestOnDirectory("valid/pairs", false, runBackendTest)
  runTestOnDirectory("valid/runtimeErr", true, runBackendTest)
  runTestOnDirectory("valid/if", false, runBackendTest)
  runTestOnDirectory("valid/variables", false, runBackendTest)
  runTestOnDirectory("valid/sequence", false, runBackendTest)
  runTestOnDirectory("valid/expressions", false, runBackendTest)
  runTestOnDirectory("valid/while", false, runBackendTest)
  runTestOnDirectory("valid/IO", false, runBackendTest)
  runTestOnDirectory("valid/scope", false, runBackendTest)
}
