package wacc

import wacc.Types._
import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

object OuterBodySymbolTable {
  def apply(scoper: Scoper): OuterBodySymbolTable = {
    OuterBodySymbolTable(scoper,
                         Map[(String, Int), (SAType, Int)](),
                         4,
                         Stack[Int](),
                         Set[(String, Int)](),
                         true)
  }
}

case class OuterBodySymbolTable private (
    var scoper: Scoper,
    private val map: Map[(String, Int), (SAType, Int)],
    private var totalOffset: Int,
    private val scopeSizes: Stack[Int],
    private val seen_set: Set[(String, Int)],
    private var mutable: Boolean) {
  import wacc.AST.Identifier
  import wacc.Errors.{Error, UndeclaredVariableError, RedeclaredVariableError}

  override def toString(): String = {
    map.toString()
  }

  def deepcopy(): OuterBodySymbolTable = {
    val newMap = Map[(String, Int), (SAType, Int)]()
    for ((k, v) <- map) {
      newMap += k -> v
    }
    OuterBodySymbolTable(
      new Scoper,
      newMap,
      totalOffset,
      scopeSizes,
      seen_set,
      mutable
    )
  }

  def enterScope() = {
    if (scoper.getScope() == 0) {
      totalOffset = -4
    }
    scopeSizes.push(totalOffset)
    scoper.enterScope()
  }

  def exitScope() = {
    totalOffset = scopeSizes.pop()
    scoper.exitScope()
  }

  private def lookup(identifier: Identifier): Option[(SAType, Int)] = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (mutable && map.contains(key) || !mutable && seen_set.contains(key)) {
        return Some(map(key))
      }
    }
    return None
  }

  def lookupVarType(
      identifier: Identifier
  )(implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    lookup(identifier) match {
      case Some((varType, _)) =>
        Left(varType)
      case default =>
        Right(
          errorList :+ UndeclaredVariableError(
            identifier.pos,
            identifier.name
          )
        )
    }
  }

  def lookupScope(identifier: Identifier): Int = {
    val iter = scoper.getIterator()
    while (iter.hasNext) {
      val curr = iter.next()
      val key = (identifier.name, curr)
      if (mutable && map.contains(key) || !mutable && seen_set.contains(key)) {
        return key._2
      }
    }
    throw new Exception("variable " + identifier + "cannot be found")
  }

  def lookupType(identifier: Identifier): SAType = {
    lookupVarType(identifier)(Nil) match {
      case Left(x) => x
      case _ =>
        throw new Exception("variable " + identifier + "cannot be found")
    }
  }

  def insertVar(identifier: Identifier, t: SAType)(
      implicit
      errorList: List[Error]): List[Error] = {
    if (!mutable)
      throw new Exception("SymbolTable has been locked; cannot be added to")
    val key = (identifier.name, scoper.getScope())
    if (map.contains(key))
      return errorList :+ RedeclaredVariableError(
        identifier.pos,
        identifier.name
      )

    if (scoper.getScope() == 0) {
      map += (key -> (t, totalOffset))
      totalOffset += 4 // arm calling convention is all pushes are 4 bytes, all parameters are max 4 bytes
    } else {
      totalOffset -= getNoBytes(t)
      map += (key -> (t, totalOffset))
    }

    errorList
  }

  // Resets scoper object and makes the object immutable
  def resetScope(): Unit = {
    scoper = new Scoper
    mutable = false
  }

  def encountered(identifier: Identifier): Unit = {
    val key = (identifier.name, scoper.getScope())
    seen_set += key
  }

  def lookupAddress(identifier: Identifier): Int = {
    lookup(identifier) match {
      case Some((_, offset)) => offset
      case _                 => throw new Exception("identifier cannot be found")
    }
  }

  // Scope 0 is reserved for parameters of a function; positive as they are stored above FP
  def getFrameSize(): Int =
    if (map.isEmpty) 0 else -Math.min(0, map.minBy(_._2._2)._2._2)
}
