package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Parser.parseAsProgram
import IR.getInlinedFunctions

class InlineFunctionsUnitTests extends AnyFlatSpec {
  "getInlinedFunctions" should "inline small functions" in {
    val program = parseAsProgram("begin int foo() is return 1 end int bar() is return 2 end int x = call foo() ; int y = call bar() end")
    val inlinedFunctions = getInlinedFunctions(program)
    inlinedFunctions.keySet should be {
        Set("foo", "bar")
    }
  }

  "getInlinedFunctions" should "inline functions with a small number of function calls" in {
    val program = parseAsProgram("begin int foo() is return 1 end int x = call foo() ; int y = call foo() ; int z = call foo() end")
    val inlinedFunctions = getInlinedFunctions(program)
    inlinedFunctions.keySet should be {
        Set("foo")
    }
  }

  "getInlinedFunctions" should "inline small functions that have a large number of function calls" in {
    val program = parseAsProgram("begin int foo() is int a = 1; return 1 end int b = call foo() ; int c = call foo() ; int d = call foo() ; int e = call foo() ; int f = call foo() ; int g = call foo() end")
    val inlinedFunctions = getInlinedFunctions(program)
    inlinedFunctions.keySet should be {
        Set("foo")
    }
  }

  "getInlinedFunctions" should "inline large functions that have a small number of function calls" in {
    val program = parseAsProgram("begin int foo() is int a = 1; int b = 2; int c = 3; int d = 4; int e = 5; int f = 6; return 1 end int g = call foo() ; int h = call foo() end")
    val inlinedFunctions = getInlinedFunctions(program)
    inlinedFunctions.keySet should be {
        Set("foo")
    }
  }

  "getInlinedFunctions" should "not inline large functions that have a large number of function calls" in {
    val program = parseAsProgram("begin int foo() is int a = 1; int b = 2; int c = 3; int d = 4; int e = 5; int f = 6; return 1 end int g = call foo() ; int h = call foo() ; int i = call foo() ; int j = call foo() ; int k = call foo() ; int l = call foo() end")
    val inlinedFunctions = getInlinedFunctions(program)
    inlinedFunctions.keySet should be {
        Set()
    }
  }

  "getInlinedFunctions" should "not inline nested functions" in {
    val program = parseAsProgram("begin int foo() is int x = call bar () ; return 1 end int bar() is return 1 end int x = call foo() end")
    val inlinedFunctions = getInlinedFunctions(program)
    inlinedFunctions.keySet should be {
        Set("bar")
    }
  }
}
