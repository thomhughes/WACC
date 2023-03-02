package wacc

case class SymbolTable() {
  import scala.collection.mutable.Map
  import wacc.OuterBodySymbolTable
  import wacc.Types._
  import wacc.AST.Identifier
  import wacc.Errors.Error

  val map = Map[String, OuterBodySymbolTable]()

  override def toString(): String = {
    map.toString()
  }

  def lookupVarType(identifier: Identifier)(implicit
      errorList: List[Error],
      funcName: String
  ): Either[SAType, List[Error]] = {
    if (map.contains(funcName)) {
      return map(funcName).lookupVarType(identifier)
    }
    throw new Exception(map.toString() + "," + funcName + ": can't be found.")
  }

  def lookupType(identifier: Identifier)(implicit funcName: String): SAType = {
    if (map.contains(funcName)) {
      return map(funcName).lookupType(identifier)
    }
    throw new Exception(map.toString() + "," + funcName + ": can't be found.")
  }

  def insertFunction(funcName: String): Unit =
    map += funcName -> new OuterBodySymbolTable(new Scoper)

  def insertVar(identifier: Identifier, t: SAType)(implicit
      errorList: List[Error],
      funcName: String
  ): List[Error] = {
    if (map.contains(funcName)) {
      return map(funcName).insertVar(identifier, t)
    }
    throw new Exception(map.toString() + "," + funcName + ": can't be found.")
  }

  def resetScope()(implicit funcName: String) =
    map(funcName).updateScoper(new Scoper)

  def enterScope()(implicit funcName: String) =
    map(funcName).enterScope()
  def exitScope()(implicit funcName: String) = map(funcName).exitScope()
  def getScope()(implicit funcName: String) = map(funcName).scoper.getScope()

  def lookupAddress(identifier: Identifier)(implicit funcName: String): Int = {
    if (map.contains(funcName)) {
      map(funcName).lookupAddress(identifier)
    } else {
      throw new Exception(map.toString() + "," + funcName + ": can't be found.")
    }
  }

  def getFrameSize()(implicit funcName: String): Int = {
    if (map.contains(funcName)) {
      map(funcName).getFrameSize()
    } else {
      throw new Exception(map.toString() + "," + funcName + ": can't be found.")
    }
  }

  def encountered(identifier: Identifier)(implicit funcName: String): Unit =
    map(funcName).encountered(identifier)
}
