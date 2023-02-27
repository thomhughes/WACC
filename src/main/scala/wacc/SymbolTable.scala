package wacc

case class SymbolTable(var scoper: Scoper) {
  import scala.collection.mutable.Map
  import wacc.Types._
  import wacc.AST.Identifier
  import wacc.Errors.{Error, UndeclaredVariableError, RedeclaredVariableError}

  val map = Map[(String, Int), (SAType, Option[Int])]()
  val memMap = Map[Int, Int]().withDefaultValue(0)

  override def toString(): String = {
    map.toString()
  }

  def lookupVarType(identifier: Identifier)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (map.contains(key)) return Left(map(key)._1)
    }
    Right(errorList :+ UndeclaredVariableError(identifier.pos, identifier.name))
  }

    def lookupType(identifier: Identifier): SAType = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (map.contains(key)) return map(key)._1
    }
    throw new Exception("variable cannot be found")
  }

  def insertVar(identifier: Identifier, t: SAType)(
      implicit errorList: List[Error]): List[Error] = {
    val key = (identifier.name, scoper.getScope())
    if (map.contains(key))
      return errorList :+ RedeclaredVariableError(identifier.pos,
                                                  identifier.name)
    map += (key -> (t, None))
    errorList
  }

  def updateScoper(newScoper: Scoper) = scoper = newScoper

  def updateVar(identifier: Identifier, scopeNo: Int, bytes: Int) = {
    val key = (identifier.name, scopeNo)
    if (!map.contains(key)) throw new Exception("unexpected variable being looked up or scope is not behaving as expected")
    memMap(scopeNo) += bytes
    val newVal = (map(key)._1, Some(memMap(scopeNo)))
    map += (key -> newVal)
  }

  def lookupVarNo(identifier: Identifier): (Int, Int) = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (map.contains(key)) return (memMap(curr), map(key)._2.get)
    }
    throw new Exception("Variable does not exist in scope")
  }

  def lookupAddress(identifier: Identifier): Int = {
    val (scope, offset) = lookupVarNo(identifier)
    return scope - offset
  }
}
