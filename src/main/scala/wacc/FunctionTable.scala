package wacc

import scala.collection.mutable.Map
import wacc.Types._

class FunctionTable {
  import wacc.AST.{Func, Identifier}
  import wacc.Errors.{Error, UndeclaredFunctionError, RedeclaredFunctionError}
  import wacc.Analyser.convertSyntaxToTypeSys
  import scala.collection.Set

  val map = Map[String, (Map[TypeSignature, Int], Int)]()

  def checkDuplicate(function: Func, typeSignature: TypeSignature) =
    map.get(function.identBinding.identifier.name) match {
      case Some((map, _)) =>
        map.contains(typeSignature)
      case None => false
    }

  def getFunctionNames = map.keySet

  def insertFunction(function: Func, typeSignature: TypeSignature)(
      implicit errorList: List[Error]): List[Error] = {
    val funcName = function.identBinding.identifier.name
    if (checkDuplicate(function, typeSignature))
      errorList :+ RedeclaredFunctionError(
        function.pos,
        funcName)
    else {
      if (!map.contains(funcName)) {
        map += (funcName -> (Map(typeSignature -> 0), 1))
      } else {
        val (innerMap, no) = map(funcName)
        innerMap += (typeSignature -> no)
        map += (funcName -> (innerMap, no + 1))
      }
      errorList
    }
  }

  def getFunctionRet(function: Func)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    if (map.contains(function.identBinding.identifier.name)) {
      return Left(convertSyntaxToTypeSys(function.identBinding.typeName))
    }
    Right(
      errorList :+ UndeclaredFunctionError(
        function.pos,
        function.identBinding.identifier.name))
  }

  def getAllowedTypeSignatures(identifier: Identifier)(implicit errorList: List[Error])
    : Either[Set[TypeSignature], List[Error]] = {
    if (map.contains(identifier.name)) {
      return Left(map(identifier.name)._1.keySet)
    }
    Right(errorList :+ UndeclaredFunctionError(identifier.pos, identifier.name))
  }

  def getFunctionNo(funcName: String, typeSignature: TypeSignature): Int = {
    map.get(funcName).get._1.get(typeSignature).get // change this
  }

  def containsFunction(funcName: String) = map.contains(funcName)

  override def toString(): String = map.toString()
}
