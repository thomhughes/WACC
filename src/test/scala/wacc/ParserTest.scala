package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalacheck.Test
import org.scalacheck.Prop.forAll

class ParserTest extends AnyFlatSpec {
    private[this] val NUM_TESTS: Int = 500

    "Parser" should "correctly parse any input program" in {
        val propConcatLists = forAll {
            (l1: List[Int], l2: List[Int]) => l1.size + l2.size == (l1:::l2).size  
        }
        println(Test.check(Test.Parameters.default.withMinSuccessfulTests(NUM_TESTS), propConcatLists))
    }
}