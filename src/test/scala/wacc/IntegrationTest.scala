package wacc

import org.scalatest.flatspec.AnyFlatSpec

class IntegrationTest extends AnyFlatSpec {
    import java.nio.file.{Files, Paths, Path}
    import scala.jdk.CollectionConverters._
    import org.scalatest.matchers.should.Matchers._
    import scala.sys.process._

    val directoriesToRunTestsOn = List("invalid/syntaxErr")

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
