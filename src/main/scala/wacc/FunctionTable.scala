package wacc

import scala.collection.mutable.Map
import wacc.Types._

class FunctionTable {
  import wacc.AST.{Func, Identifier}
  import wacc.Errors.{Error, UndeclaredFunctionError, RedeclaredFunctionError}

  val map = Map[String, (Types.SAType, List[Types.SAType])]()

  def checkDuplicate(function: Func) =
    map.contains(function.identBinding.identifier.name)

  def insertFunction(function: Func, params: (SAType, List[SAType]))(
      implicit errorList: List[Error]): List[Error] = {
    if (checkDuplicate(function))
      errorList :+ RedeclaredFunctionError(
        function.pos,
        function.identBinding.identifier.name)
    else {
      map += (function.identBinding.identifier.name -> params)
      errorList
    }
  }

  def getFunctionRet(function: Func)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    if (map.contains(function.identBinding.identifier.name)) {
      val (retType, params) = map(function.identBinding.identifier.name)
      return Left(retType)
    }
    Right(
      errorList :+ UndeclaredFunctionError(
        function.pos,
        function.identBinding.identifier.name))
  }

  def getFunctionEntry(identifier: Identifier)(implicit errorList: List[Error])
    : Either[(SAType, List[SAType]), List[Error]] = {
    if (map.contains(identifier.name)) {
      return Left(map(identifier.name))
    }
    Right(errorList :+ UndeclaredFunctionError(identifier.pos, identifier.name))
  }

  def getFunctionParams(identifier: Identifier)(
      implicit errorList: List[Error]): Either[List[SAType], List[Error]] = {
    if (map.contains(identifier.name)) {
      val (retType, params) = map(identifier.name)
      return Left(params)
    }
    Right(errorList :+ UndeclaredFunctionError(identifier.pos, identifier.name))
  }

  def containsFunction(funcName: String) = map.contains(funcName)

  override def toString(): String = map.toString()
}
