package wacc

import parsley.{Success, Failure}
import wacc.Parser.parse
// import wacc.Parser.parseExpression
import wacc.IO.readFile

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
                if (args.length >= 2 && args(1) == "--debug") {
                    println(x)
                    println("exit:\n0")
                }
                sys.exit(0)
            }
            case Failure(msg) => {
                if (args.length >= 2) {
                    if (args(1) != "--suppress") {
                        System.err.println("#syntax error#")
                    } else if (args(1) == "--debug") {
                        println(msg)
                        System.err.println("exit:\n100")
                    }
                }
                sys.exit(100)                   
            }
        }
    }
}
 