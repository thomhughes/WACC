package wacc

import scala.collection.mutable.Map

case class SymbolTable private (private val map: Map[String, OuterBodySymbolTable]) {
  import wacc.OuterBodySymbolTable
  import wacc.Types._
  import wacc.AST.Identifier
  import wacc.Errors.Error

  override def toString(): String = {
    map.toString()
  }

  def getOuterBodySymbolTable(funcName: String): Option[OuterBodySymbolTable] =
    if (map.contains(funcName)) Some(map(funcName)) else None

  def deepcopy(): SymbolTable = {
    val newMap = Map[String, OuterBodySymbolTable]()
    for ((k, v) <- map) {
      newMap += k -> v.deepcopy()
    }
    SymbolTable(newMap)
  }

  // Used during SA; uses ErrorList
  def lookupVarType(identifier: Identifier)(
      implicit
      errorList: List[Error],
      funcName: String): Either[SAType, List[Error]] = {
    if (map.contains(funcName)) {
      return map(funcName).lookupVarType(identifier)
    }
    throw new Exception(map.toString() + "," + funcName + ": can't be found.")
  }

  // Used after SA; does not use ErrorList
  def lookupScope(identifier: Identifier)(implicit funcName: String): Int = {
    if (map.contains(funcName)) {
      return map(funcName).lookupScope(identifier)
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
    map += funcName -> OuterBodySymbolTable(new Scoper)

  def insertVar(identifier: Identifier, t: SAType)(
      implicit
      errorList: List[Error],
      funcName: String): List[Error] = {
    if (map.contains(funcName)) {
      return map(funcName).insertVar(identifier, t)
    }
    throw new Exception(map.toString() + "," + funcName + ": can't be found.")
  }

  def resetScope()(implicit funcName: String) =
    map(funcName).resetScope()

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

case object SymbolTable {
  def apply(): SymbolTable = SymbolTable(Map[String, OuterBodySymbolTable]())
}