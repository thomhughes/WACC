package wacc

object TypeConverter {
    import AST._
    import Types._
    def convertSyntaxToTypeSys(t: ASTType): SAType = {
        t match {
            case IntType => SAIntType
            case BoolType => SABoolType
            case CharType => SACharType
            case StringType => SAStringType
            case ArrayType(arrayType, arity) => SAArrayType(convertSyntaxToTypeSys(arrayType), arity)
            case PairType(fstType, sndType) => SAPairType(convertSyntaxToTypeSys(fstType), convertSyntaxToTypeSys(sndType))
            case PairRefType => throw new Exception("PairRefType should not be used in the type system")
        }
    }
}