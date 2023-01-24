package wacc

import parsley.{Parsley, Success, Failure}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_42!")

        lazy val integer = digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)

        val add = (x: BigInt, y: BigInt) => x + y
        val sub = (x: BigInt, y: BigInt) => x - y

        lazy val expr: Parsley[BigInt] =
            chain.left1[BigInt](
                ('(' ~> expr <~ ')') <|> integer,
                ('+' #> add) <|> ('-' #> sub)
            )

        expr.parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
    }
}

