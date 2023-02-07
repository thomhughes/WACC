package wacc

import parsley.{Success, Failure}
import wacc.Parser.parse
import wacc.IO.readFile

object Main {
    def main(args: Array[String]): Unit = {
        parse(readFile(args.head)) match {
            case Success(x) => {
                if (checkProgram(x)) {
                    if (args.length >= 2 && args(1) == "--debug") {
                        println(x)
                        println("exit:\n0")
                    }
                    sys.exit(0)
                }
                if (args.length >= 2) {
                    if (args(1) != "--suppress") {
                        System.err.println("#semantic error#")
                    } else if (args(1) == "--debug") {
                        println(msg)
                        System.err.println("exit:\n200")
                    }
                }
                sys.exit(200)
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
 