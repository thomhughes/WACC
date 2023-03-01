package wacc

case class OuterBodySymbolTable(var scoper: Scoper) {
  import scala.collection.mutable.Map
  import scala.collection.mutable.Stack
  import wacc.Types._
  import wacc.AST.Identifier
  import wacc.Errors.{Error, UndeclaredVariableError, RedeclaredVariableError}

  val map = Map[(String, Int), (SAType, Int)]()

  var totalOffset = 4 // FP, LR backup is on stack
  var frameSize = 0
  val scopeSizes = Stack[Int]()

  override def toString(): String = {
    map.toString()
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
      if (map.contains(key)) {
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

  def lookupType(identifier: Identifier): SAType = {
    lookupVarType(identifier)(Nil) match {
      case Left(x) => x
      case _ =>
        throw new Exception("variable " + identifier + "cannot be found")
    }
  }

  def insertVar(identifier: Identifier, t: SAType)(implicit
      errorList: List[Error]
  ): List[Error] = {
    val key = (identifier.name, scoper.getScope())
    if (map.contains(key))
      return errorList :+ RedeclaredVariableError(
        identifier.pos,
        identifier.name
      )

    val noBytes = t match {
      case SAIntType | SAArrayType(_, _) | SAPairType(_, _) | SAStringType | SAUnknownType => 4
      case SABoolType | SACharType                          => 1
      case _ => throw new Exception("Unexpected LValue type: " + t.toString())
    }

    if (scoper.getScope() == 0) {
      map += (key -> (t, totalOffset))
      totalOffset += 4 // arm calling convention is all pushes are 4 bytes, all parameters are max 4 bytes
    } else {
      totalOffset -= noBytes
      map += (key -> (t, totalOffset))
    }

    errorList
  }

  def updateScoper(newScoper: Scoper): Unit = scoper = newScoper

  def lookupAddress(identifier: Identifier): Int = {
    lookup(identifier) match {
      case Some((_, offset)) => offset
      case _ => throw new Exception("identifier cannot be found")
    }
  }

  def getFrameSize(): Int =
    if (map.isEmpty) 0 else -Math.min(0, map.minBy(_._2._2)._2._2)
}
