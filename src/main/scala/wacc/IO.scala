package wacc

object IO {
  import scala.io.Source
  import scala.io.Codec

  def readFile(fileName: String): String = {
    implicit val codec: Codec = Codec.UTF8
    val source = Source.fromFile(fileName)
    try source.getLines().mkString("\n")
    finally source.close
  }
}
