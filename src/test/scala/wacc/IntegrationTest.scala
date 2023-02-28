package wacc

import org.scalatest.flatspec.AnyFlatSpec

class IntegrationTest extends AnyFlatSpec {
  import java.nio.file.{Files, Paths, Path}
  import scala.io.Source
  import scala.jdk.CollectionConverters._
  import org.scalatest.matchers.should.Matchers._
  import scala.sys.process._

  def runFrontendTest(path: Path, run: Boolean): Unit = {
    if (!path.toString().endsWith(".wacc")) {
      return
    }
    var exitCode = 0
    if (path.toString().startsWith("wacc_examples/invalid/syntaxErr")) {
      exitCode = 100
    } else if (
      path.toString().startsWith("wacc_examples/invalid/semanticErr")
    ) {
      exitCode = 200
    }
    if (run) {
      val result = ("./compile " + path.toString() + " --suppress").!
      "Parsing " + path
        .toString() + " test case" should "have exit code " + exitCode.toString in {
        result shouldBe (exitCode)
      }
    } else {
      "Parsing " + path
        .toString() + " test case" should "have exit code " + exitCode.toString in pending
    }
  }

  def runBackendTest(path: Path, run: Boolean): Unit = {
    if (!path.toString().endsWith(".wacc")) {
      return
    }
    if (run) {
      println("Running " + path.toString())
      val compileExitCode = ("./compile " + path.toString() + " --suppress").!
      if (compileExitCode != 0) {
        throw new RuntimeException(
          "Compiling " + path.toString() + " led to a semantic or syntax error"
        )
      }
      val inputFileName =
        path.toString().substring(path.toString().lastIndexOf("/") + 1)
      val executablePath = Paths.get(inputFileName.replaceFirst(".wacc$", ""))
      val assemblyPath = Paths.get(inputFileName.replaceFirst(".wacc$", ".s"))
      val assemblerExitCode =
        ("arm-linux-gnueabi-gcc -o " + executablePath.toString + " -mcpu=arm1176jzf-s -mtune=arm1176jzf-s " + assemblyPath.toString).!
      if (assemblerExitCode != 0) {
        throw new RuntimeException(
          "Assembling " + path.toString() + " led to an error"
        )
      }
      val programInputLines = Source
        .fromFile(path.toString)
        .getLines()
        .toList
        .dropWhile(x => !x.replace(" ", "").startsWith("#Input:"))

      val exitCodeLines = Source
        .fromFile(path.toString)
        .getLines()
        .toList
        .dropWhile(x => !x.replace(" ", "").startsWith("#Exit:"))

      var executeCommand =
        "qemu-arm -L /usr/arm-linux-gnueabi/ " + executablePath.toString
      if (programInputLines.length != 0) {
        val programInputLine = programInputLines.head
        val programInput =
          programInputLine.substring(programInputLine.indexOf(':') + 2)
        executeCommand =
          "sh -c \"echo \'" + programInput + "\' | qemu-arm -L /usr/arm-linux-gnueabi/ " + executablePath.toString + "\""
      }
      if (exitCodeLines.length > 1) {
        val exitCodeLine = exitCodeLines.tail.head
        val exitCode = exitCodeLine.substring(1).trim().toInt
        "Backend " should "correctly generate code for " + path
          .toString() + " test case" in {
          executeCommand.! shouldBe (exitCode)
        }
      } else {
        val actualOutput = executeCommand.!!
        val exampleOutput =
          Source
            .fromFile(path.toString)
            .getLines()
            .toList
            .dropWhile(x => !x.equals("# Output:"))
            .takeWhile(x => !x.equals(""))
            .map(x => x.substring(1).strip())
            .tail
            .mkString("\n")
        "Backend " should "correctly generate code for " + path
          .toString() + " test case" in {
          assert(exampleOutput == actualOutput)
        }
      }
    } else {
      "Backend " should "correctly generate code for " + path
        .toString() + " test case" in pending
    }
  }

  def runTestOnDirectory(
      path: String,
      run: Boolean,
      test: (Path, Boolean) => Unit
  ): Unit =
    Files
      .walk(Paths.get("wacc_examples/" + path))
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .foreach((x) => test(x, run))
}
