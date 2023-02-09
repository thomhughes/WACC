package wacc

import scala.collection.mutable.Stack

class Scoper {
  var curr = 0
  var next = 0
  val stack = Stack[Int](0)

  def enterScope() = {
    next += 1
    curr = next
    stack.push(curr)
  }

  def getScope() = curr

  def exitScope() = {
    stack.pop()
    curr = stack.top
  }

  def getIterator() = stack.iterator
}
