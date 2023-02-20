package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.IR._
import wacc.Parser._
import scala.collection.mutable.ListBuffer

class IRConversionUnitTests extends AnyFlatSpec {
    "IR Conversion" should "generate the correct IR for an AST for a basic declaration" in {
        buildIR(parseAsProgram("begin int x = 3 end")) should be (ListBuffer(Instr(STR,Some(Var("x")),Some(Imm(3)),None,AL)))
    }

    "IR Conversion" should "generate the correct IR for an AST for a simple addition" in {
        buildIR(parseAsProgram("begin int x = 3 + 4 end")) should be ()
    }

    "IR Conversion" should "generate the correct IR for an AST for a complex addition" in {

    }
}
