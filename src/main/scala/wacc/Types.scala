package wacc

object Types {
  sealed trait SAType
  case object SAAnyType extends SAType
  case object SAIntType extends SAType {
    override def toString() = "int"
  }
  case object SABoolType extends SAType {
    override def toString() = "bool"
  }
  case object SACharType extends SAType {
    override def toString() = "char"
  }
  case object SAStringType extends SAType {
    override def toString() = "string"
  }
  case class SAArrayType(arrayType: SAType, arity: Int) extends SAType {
    override def toString() = arrayType.toString() + ("[]" * arity)
  }
  case object SAPairRefType extends SAType {
    override def toString() = "pair"
  }
  case object SAUnknownType extends SAType
  case class SAPairType(val fstType: SAType, val sndType: SAType)
      extends SAType {
    override def toString() =
      "pair(" + fstType.toString() + ", " + sndType.toString() + ")"
  }

  case class TypeSignature(val retType: SAType, val paramTypes: List[SAType]) {
    override def toString() =
      "(" + 
      (if (paramTypes.isEmpty) "" else paramTypes.tail.foldLeft(paramTypes.head.toString())((acc, t) => acc + t.toString() + ", ")) +
      ") => " + retType.toString()
  }

  def equalsType(firstType: SAType, secondType: SAType): Boolean =
    firstType match {
      case SAArrayType(firstArrayType, firstArity) =>
        secondType match {
          case SAArrayType(secondArrayType, secondArity) =>
            firstArity == secondArity && equalsType(firstArrayType,
                                                    secondArrayType)
          case SAAnyType => true
          case _         => false
        }
      case SAPairType(firstFstType, firstSndType) =>
        secondType match {
          case SAPairType(secondFstType, secondSndType) =>
            (equalsType(firstFstType, secondFstType) /* || firstFstType == SAAnyType || secondFstType == SAAnyType*/ ) && (equalsType(
              firstSndType,
              secondSndType) /* || firstSndType == SAAnyType || secondSndType == SAAnyType*/ )
          case SAAnyType     => true
          case SAPairRefType => true
          case _             => false
        }
      case SAPairRefType =>
        secondType match {
          case SAPairRefType    => true
          case SAAnyType        => true
          case SAPairType(_, _) => true
          case _                => false
        }
      case SAUnknownType =>
        secondType match {
          case SAUnknownType => false
          case _             => true
        }
      case _ =>
        firstType == SAAnyType || secondType == SAAnyType || firstType == secondType
    }

  def getNoBytes(saType: SAType): Int = saType match {
    case SAIntType | SAArrayType(_, _) | SAPairType(_, _) | SAStringType |
        SAPairRefType | SAUnknownType =>
      4
    case SABoolType | SACharType => 1
    case unexpected =>
      throw new Exception("Unexpected LValue type: " + unexpected)
  }
}
