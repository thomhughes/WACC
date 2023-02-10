package wacc

case class SymbolTable(val scoper: Scoper) {
  import wacc.Types._
  import wacc.AST.Identifier
  import wacc.Errors.{Error, UndeclaredVariableError, RedeclaredVariableError}

  var map = Map[(String, Int), SAType]()

  override def toString(): String = {
    map.toString()
  }

  def lookupVar(identifier: Identifier)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (map.contains(key)) return Left(map(key))
    }
    Right(errorList :+ UndeclaredVariableError(identifier.pos, identifier.name))
  }

  def insertVar(identifier: Identifier, t: SAType)(
      implicit errorList: List[Error]): List[Error] = {
    val key = (identifier.name, scoper.getScope())
    if (map.contains(key))
      return errorList :+ RedeclaredVariableError(identifier.pos,
                                                  identifier.name)
    map += (key -> t)
    errorList
  }
}
