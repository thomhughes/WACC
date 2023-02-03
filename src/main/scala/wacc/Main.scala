package wacc

import parsley.{Success, Failure}
// import wacc.io.readFile
import wacc.parser.parse
import wacc.parser.parseExpression
import wacc.io.readFile

object Main {
    def main(args: Array[String]): Unit = {
        // parseExpression(args.head) match {
        //     case Success(x) => {
        //         println(x)
        //     }
        //     case Failure(msg) => {
        //         println(msg)
        //     }
        // }
        parse(readFile(args.head)) match {
            case Success(x) => {
                // println(x)
                // System.err.println("exit:\n0")
                sys.exit(0)
            }
            case Failure(msg) => {
                if (args(1) != "--suppress") {
                    System.err.println("#syntax error#")
                }
                // This is for us during debugging
                // println(msg)
                // System.err.println("exit:\n100")
                sys.exit(100)                   
            }
        }
    }
}
 