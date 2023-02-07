package wacc

object Types {
    sealed trait SAType
    case object SAIntType extends SAType
    case object SABoolType extends SAType
    case object SACharType extends SAType
    case object SAStringType extends SAType
    case class SAArrayType(arrayType: SAType) extends SAType
    // 
    case class SAPairType(val fstType: SAType, val sndType: SAType) extends SAType
}