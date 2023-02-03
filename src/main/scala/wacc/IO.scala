package wacc

object io {
  import scala.io.Source

  def readFile(fileName: String): String = {
    val source = Source.fromFile(fileName)
    try source.getLines.mkString("\n")
    finally source.close
  }

}
