package wacc

object TypeConverter {
    import AST._
    import Types._
    def convertSyntaxToTypeSys(t: Type with PairElemType): SAType = {
        t match {
            case IntType => SAIntType
            case BoolType => SABoolType
            case CharType => SACharType
            case StringType => SAStringType
            case ArrayType(arrayType) => SAArrayType(convertSyntaxToTypeSys(arrayType))
            case PairType(fstType, sndType) => SAPairType(convertSyntaxToTypeSys(fstType), convertSyntaxToTypeSys(sndType))
            case PairRefType => error("Placeholder for lookup to symbol table")
        }
    }
}