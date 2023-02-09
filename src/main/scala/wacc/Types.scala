package wacc

object Types {
    sealed trait SAType
    case object SAAnyType extends SAType
    case object SAIntType extends SAType
    case object SABoolType extends SAType
    case object SACharType extends SAType
    case object SAStringType extends SAType
    case class SAArrayType(arrayType: SAType, arity: Int) extends SAType
    case object SAPairRefType extends SAType
    case object SAUnknownType extends SAType
    case class SAPairType(val fstType: SAType, val sndType: SAType) extends SAType

    def equalsType(firstType: SAType, secondType: SAType): Boolean =
        firstType match {
            case SAArrayType(firstArrayType, firstArity) => secondType match {
                case SAArrayType(secondArrayType, secondArity) => firstArity == secondArity && equalsType(firstArrayType, secondArrayType)
                case SAAnyType => true
                case _ => false
            }
            case SAPairType(firstFstType, firstSndType) => secondType match {
                case SAPairType(secondFstType, secondSndType) => (equalsType(firstFstType, secondFstType)/* || firstFstType == SAAnyType || secondFstType == SAAnyType*/) && (equalsType(firstSndType, secondSndType)/* || firstSndType == SAAnyType || secondSndType == SAAnyType*/)
                case SAAnyType => true
                case SAPairRefType => true
                case _ => false
            }
            case SAPairRefType => secondType match {
                case SAPairRefType => true
                case SAAnyType => true
                case SAPairType(_, _) => true
                case _ => false
            }
            case SAUnknownType => secondType match {
                case SAUnknownType => false
                case _ => true
            }
            case _ => firstType == SAAnyType || secondType == SAAnyType || firstType == secondType
        }
}
