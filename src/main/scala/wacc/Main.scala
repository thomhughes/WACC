package wacc

import parsley.{Success, Failure}
// import wacc.io.readFile
import wacc.parser.parse
import wacc.io.readFile

object Main {
    def main(args: Array[String]): Unit = {
        parse(readFile(args.head)) match {
            case Success(x) => sys.exit(0)
            case Failure(msg) => {
                println(msg)
                sys.exit(100)                   
            }
        }
    }
}
 