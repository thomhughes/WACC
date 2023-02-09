package wacc

object Types {
    sealed trait SAType
    case object SAAnyType extends SAType
    case object SAIntType extends SAType
    case object SABoolType extends SAType
    case object SACharType extends SAType
    case object SAStringType extends SAType
    case class SAArrayType(arrayType: SAType, arity: Int) extends SAType
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
                case _ => false
            }
            case SAAnyType => {
                println("free loading")
                println(firstType)
                println(secondType)
                println("eeee")
                secondType != SAAnyType
            }
            case _ => firstType == secondType
        }
}
