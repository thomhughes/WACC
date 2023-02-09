package wacc

import org.scalatest.flatspec.AnyFlatSpec

class IntegrationTest extends AnyFlatSpec {
    import java.nio.file.{Files, Paths, Path}
    import scala.jdk.CollectionConverters._
    import org.scalatest.matchers.should.Matchers._
    import scala.sys.process._

    val directoriesToRunTestsOn = List(
        // "valid/advanced",
        // "valid/array",
        // "valid/function",
        "valid/pairs",
        // "valid/runtimeErr",
        "valid/if", 
        "valid/basic", 
        "valid/variables", 
        "valid/sequence", 
        "valid/expressions", 
        "valid/while", 
        "valid/IO", 
        "valid/scope", 
        "invalid/syntaxErr",
        // "invalid/semanticErr/array",
        // "invalid/semanticErr/function",
        // "invalid/semanticErr/multiple",
        "invalid/semanticErr/pairs",
        // "invalid/semanticErr/read",
        // "invalid/semanticErr/scope",
        "invalid/semanticErr/exit", 
        "invalid/semanticErr/if", 
        "invalid/semanticErr/print", 
        "invalid/semanticErr/variables", 
        "invalid/semanticErr/expressions", 
        "invalid/semanticErr/while", 
        "invalid/semanticErr/IO"
    )

    directoriesToRunTestsOn.foreach(runTestOnDirectory)
    
    def runTest(path: Path): Unit = {
        val result = ("./compile " + path.toString() + " --suppress").!
        var exitCode = 0
        if (path.toString().startsWith("wacc_examples/invalid/syntaxErr")) {
            exitCode = 100
        } else if (path.toString().startsWith("wacc_examples/invalid/semanticErr")) {
            exitCode = 200
        }
        "Parsing " + path.toString() + " test case" should "have exit code " + exitCode.toString in {
            result shouldBe (exitCode)
        }
    }

    def runTestOnDirectory(path: String): Unit = {
        Files.walk(Paths.get("wacc_examples/" + path)).iterator().asScala.filter(Files.isRegularFile(_)).foreach(runTest)
    }
  
}
