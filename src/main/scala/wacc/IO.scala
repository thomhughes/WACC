object IO {
  import scala.io.Source

  def readFile(filename: String): String = {
    Source.fromFile(filename).getLines.mkString
  }

}
