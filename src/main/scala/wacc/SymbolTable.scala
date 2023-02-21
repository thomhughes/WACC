package wacc

case class SymbolTable(var scoper: Scoper) {
  import scala.collection.mutable.Map
  import wacc.Types._
  import wacc.AST.Identifier
  import wacc.Errors.{Error, UndeclaredVariableError, RedeclaredVariableError}

  val map = Map[(String, Int), Either[SAType, Int]]()
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
      if (map.contains(key)) return map(key) match {
        case Left(x) => Left(x)
        case _ => throw new Exception("Type lookup has been performed after updating")
      }
    }
    Right(errorList :+ UndeclaredVariableError(identifier.pos, identifier.name))
  }

  def insertVar(identifier: Identifier, t: SAType)(
      implicit errorList: List[Error]): List[Error] = {
    val key = (identifier.name, scoper.getScope())
    if (map.contains(key))
      return errorList :+ RedeclaredVariableError(identifier.pos,
                                                  identifier.name)
    map += (key -> Left(t))
    errorList
  }

  def updateScoper(newScoper: Scoper) = scoper = newScoper

  def updateVar(identifier: Identifier, scopeNo: Int, bytes: Int) = {
    val key = (identifier.name, scopeNo)
    if (!map.contains(key)) throw new Exception("unexpected variable being looked up or scope is not behaving as expected")
    memMap(scopeNo) += bytes
    map += (key -> Right(memMap(scopeNo)))
  }

  def lookupVarNo(identifier: Identifier): Int = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (!map.contains(key)) return map(key) match {
        case Right(x) => x
        case _ => throw new Exception("The looked up value has no associated int")
      }
    }
    throw new Exception("Variable does not exist in scope")
  }
}
