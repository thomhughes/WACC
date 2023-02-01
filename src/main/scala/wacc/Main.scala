package wacc

import parsley.{Success, Failure}
// import wacc.io.readFile
import wacc.parser.parse
import wacc.io.readFile

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_42!")

        parse(readFile(args.head)) match {
            case Success(x) => println("exit:\n0")
            case Failure(msg) => {
                println(msg)
                println("exit:\n100")                     
            }
        }
    }
}
 