package wacc

class BackendTests extends IntegrationTest {
  runTestOnDirectory("valid/basic", true, runBackendTest)
  runTestOnDirectory("valid/array", true, runBackendTest)
  runTestOnDirectory("valid/function", false, runBackendTest)
  runTestOnDirectory("valid/pairs", true, runBackendTest)
  runTestOnDirectory("valid/runtimeErr", true, runBackendTest)
  runTestOnDirectory("valid/if", true, runBackendTest)
  runTestOnDirectory("valid/variables", true, runBackendTest)
  runTestOnDirectory("valid/sequence", true, runBackendTest)
  runTestOnDirectory("valid/expressions", true, runBackendTest)
  runTestOnDirectory("valid/while", true, runBackendTest)
  runTestOnDirectory("valid/IO", true, runBackendTest)
  runTestOnDirectory("valid/scope", true, runBackendTest)
  runTestOnDirectory("valid/overloading", false, runBackendTest)
  runTestOnDirectory("valid/fullpairtypes", true, runBackendTest)
  runTestOnDirectory("valid/stdlib", true, runBackendTest)
}
