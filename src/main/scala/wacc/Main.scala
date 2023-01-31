package wacc

import parsley.{Success, Failure}
// import wacc.io.readFile
import wacc.parser.parse

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_42!")

        parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
    }
}
