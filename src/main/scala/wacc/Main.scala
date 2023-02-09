package wacc

import parsley.{Success, Failure}
import wacc.Parser.parse

object Main {
    import java.io.File

    def main(args: Array[String]): Unit = {
      try
        parse(new File(args.head)).get match {
          case Success(x) => {
              if (args.length >= 2 && args(1) == "--debug") {
                  println(x)
                  println("exit:\n0")
              }
              sys.exit(0)
          }
          case Failure(msg) => {
              System.err.println(msg)
              sys.exit(100)                   
          }
        }
      finally
        println("Finished.")
    }
}
 
