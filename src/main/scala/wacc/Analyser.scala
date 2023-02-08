package wacc

object Analyser {
    import wacc.AST._
    import wacc.Types._
    import wacc.TypeConverter.convertSyntaxToTypeSys
    import wacc.Keywords.keywords

    val scoper = new Scoper()
    val functionTable = Map[String, (SAType, List[SAType])]()
    val symbolTable = Map[(String, Int), SAType]()
    val errorList = List[String]()

    private def checkUnOp(op: UnaryOp, expression: Expression): Boolean = {
        op match {
            case Chr => checkExpression(expression, SAIntType)
            case Len => expression match {
                case Identifier(id) => lookupVar(id) match {
                    case Some(SAArrayType(_, _)) => true
                    case default => false
                }
                case ArrayElem(id, indices) => lookupVar(id.name) match {
                    case Some(SAArrayType(_, arity)) => indices.length < arity
                    case default => false
                }
                case default => false
            }
            case Negation => checkExpression(expression, SAIntType)
            case Not => checkExpression(expression, SABoolType)
            case Ord => checkExpression(expression, SACharType)
        }
    }

    private def checkBinOp(op: BinaryOp, lhs: Expression, rhs: Expression): Boolean = {
        op match {
            case And | Or => checkExpression(lhs, SABoolType) && checkExpression(rhs, SABoolType)
            case default => checkExpression(lhs, SAIntType) && checkExpression(rhs, SAIntType)
        }
    }

    private def checkExpression(expression: Expression, t: SAType): Boolean = {
        expression match {
            case IntLiteral(_) => equals(t, SAIntType)
            case BoolLiteral(_) => equals(t, SABoolType)
            case CharLiteral(_) => equals(t, SACharType)
            case StringLiteral(_) => equals(t, SAStringType)
            case PairLiteral => t match {
                case SAPairType(_, _) => true
                case default => false
            }
            case Identifier(id) => lookupVar(id).isDefined && equals(t, lookupVar(id).get)
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(typeName) => equals(t, typeName)
                case None => false
            }
            case UnaryOpApp(op, expr) => checkUnOp(op, expr)
            case BinaryOpApp(op, lhs, rhs) => checkBinOp(op, lhs, rhs)
            case default => false
        }
    }

    def checkProgram(program: Program) =
        program.functions.forall(checkFunction) && program.statements.forall(checkStatement)
    
    // TODO: implement checkFunction
    private def checkFunction(func: Func) = true

    private def checkStatement(statement: Statement): Boolean = {
        statement match {
            case SkipStatement => true
            case DeclarationStatement(typeName, identifier, rvalue) => checkDeclarationStatement(convertSyntaxToTypeSys(typeName), identifier, rvalue)
            case AssignmentStatement(lvalue, rvalue) => checkAssignmentStatement(lvalue, rvalue)
            case ReadStatement(lvalue) => checkReadStatement(lvalue)
            case FreeStatement(expression) => checkFreeStatement(expression)
            case ReturnStatement(expression) => false
            case ExitStatement(expression) => checkExitStatement(expression)
            case PrintStatement(expression) => checkPrintStatement(expression)
            case PrintLnStatement(expression) => checkPrintLnStatement(expression)
            case IfStatement(condition, thenStatements, elseStatements) => checkIfStatement(condition, thenStatements, elseStatements)
            case WhileStatement(condition, doStatements) => checkWhileStatement(condition, doStatements)
            case BeginStatement(statements) => checkBeginStatement(statements)
        }
    }

    private def checkDeclarationStatement(typeName: SAType, identifier: Identifier, rvalue: RValue) = {
        val idenIsKeyword = keywords.contains(identifier.name)
        if (idenIsKeyword) {
            ("Identifier " + identifier.name + " is a WACC keyword") :: errorList
            false
        }
        val idenNotInSymTable = insertVar(identifier.name, typeName)
        if (idenNotInSymTable) {
            if (checkRValue(rvalue, typeName)) {
                ("RHS of declaration statment is not of type " + typeName) :: errorList
            } else {
                true
            }
        } else {
            ("Identifier " + identifier.name + " already declared in the current scope") :: errorList
        }
        false
    }
    
    private def checkRValue(rvalue: RValue, typeName: SAType): Boolean = {
        rvalue match {
            case NewPair(fst, snd) => typeName match {
                case SAPairType(fstType, sndType) => equals(fstType, fst) && equals(snd, sndType)
                case default => false
            }
            case ArrayLiteral(list) => typeName match {
                case SAArrayType(arrayType: SAType, _) => list.forall((x) => checkExpression(x, arrayType))
                case _ => false   
            }
            case PairElem(index, pair) => checkPairElem(index, pair, typeName)
            // TODO
            case FunctionCall(id, args) => ???
            case expr:Expression => checkExpression(expr, typeName)
            case default => false
        }
    }

    private def checkPairElem(index: PairIndex, pair: LValue, typeName: SAType): Boolean = {
        pair match {
            case Identifier(id) => lookupVar(id) match {
                case Some(SAPairType(fstType, sndType)) => index match {
                    case Fst => equals(fstType, typeName)
                    case Snd => equals(sndType, typeName)
                }
                case default => false
            }
            case PairElem(anotherIndex, anotherPair) => index match {
                case Fst => checkPairElem(anotherIndex, anotherPair, SAPairType(typeName, SAAnyType))
                case Snd => checkPairElem(anotherIndex, anotherPair, SAPairType(SAAnyType, typeName))
            }
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(SAPairType(fstType, sndType)) => index match {
                    case Fst => equals(fstType, typeName)
                    case Snd => equals(sndType, typeName)
                }
                case _ => false
            }
        }
        true
    }

    private def checkAssignmentStatement(lvalue: LValue, rvalue: RValue): Boolean = {
        val typeName = lvalue match {
            case Identifier(id) => lookupVar(id)
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
            case PairElem(index, pair) => getPairElemType(index, pair)
        }
        typeName match {
            case Some(typeName) => checkRValue(rvalue, typeName)
            case default => false
        }
    }

    private def getPairElemType(index: PairIndex, pair: LValue): Option[SAType] = {
        pair match {
            case Identifier(id) => lookupVar(id) match {
                case Some(SAPairType(fstType, sndType)) => index match {
                    case Fst => Some(fstType)
                    case Snd => Some(sndType)
                }
                case default => None
            }
            case PairElem(anotherIndex, anotherPair) => index match {
                case Fst => getPairElemType(anotherIndex, anotherPair)
                case Snd => getPairElemType(anotherIndex, anotherPair)
            }
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(SAPairType(fstType, sndType)) => index match {
                    case Fst => Some(fstType)
                    case Snd => Some(sndType)
                }
                case _ => None
            }
        }
    }

    private def checkLValue(lvalue: LValue, typeName: SAType): Boolean = {
        lvalue match {
            case Identifier(id) => lookupVar(id) match {
                case Some(t) => t == typeName
                case default => false
            }
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(t) => t == typeName
                case default => false
            }
            case PairElem(index, innerLValue) => checkPairElem(index, innerLValue, typeName)
            case default => false
        }
    }

    private def getArrayElemType(id: Identifier, indices: List[Expression]): Option[SAType] =
        lookupVar(id.name) match {
            case Some(SAArrayType(arrayType, arity)) => {
                if (indices.length < arity)
                    Some(SAArrayType(arrayType, arity - indices.length))
                else if (indices.length == arity)
                    Some(arrayType)
                else 
                    None
            }
            case _ => None
        }

    private def checkExitStatement(expression: Expression) = checkExpression(expression, SAIntType)

    // TODO: change AST node to include print type info with inferType
    private def checkPrintStatement(expression : Expression) = isValidExpression(expression)

    // TODO: change AST node to include print type info with inferType
    private def checkPrintLnStatement(expression: Expression) = isValidExpression(expression)

    private def isValidExpression(expression: Expression) =
        expression match {
            case IntLiteral(_) | BoolLiteral(_) | CharLiteral(_) | StringLiteral(_) | PairLiteral => true
            case Identifier(id) => lookupVar(id).isDefined
            case ArrayElem(id, indices) => getArrayElemType(id, indices).isDefined
            case UnaryOpApp(op, expr) => checkUnOp(op, expr)
            case BinaryOpApp(op, lhs, rhs) => checkBinOp(op, lhs, rhs)
        }
    
    // TODO: change AST node to include print type info with inferType
    // need to check whether pairelem, arrayelem, var is either int or char
    private def checkReadStatement(lvalue: LValue): Boolean =
        checkLValue(lvalue, SAIntType) || checkLValue(lvalue, SACharType)
    
    private def checkFreeStatement(expression: Expression): Boolean = 
        isExpressionArrayType(expression) || isExpressionPairType(expression)
    
    private def isExpressionArrayType(expression: Expression): Boolean = {
        val typeName = expression match {
            case Identifier(id) => lookupVar(id) 
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
        }
        typeName match {
            case Some(SAArrayType(_, _)) => true
            case default => false
        }
    }

    private def isExpressionPairType(expression: Expression): Boolean = {
        val typeName = expression match {
            case Identifier(id) => lookupVar(id)
            case PairLiteral => return true
            case ArrayElem(id, indices) => getArrayElemType(id, indices) 
        }
        typeName match {
            case Some(SAPairType(_, _)) => true
            case default => false
        }
    }

    private def checkWhileStatement(condition: Expression, doStatements: List[Statement]) = {
        if (!checkExpression(condition, SABoolType)) false
        scoper.enterScope()
        if (!doStatements.forall(checkStatement)) false
        scoper.exitScope()
        true
    }

    private def checkIfStatement(condition: Expression, thenStatements: List[Statement], elseStatements: List[Statement]) = {
        if (!checkExpression(condition, SABoolType)) false
        scoper.enterScope()
        if (!thenStatements.forall(checkStatement)) false
        scoper.exitScope()
        scoper.enterScope()
        if (!elseStatements.forall(checkStatement)) false
        scoper.exitScope()
        true
    }

    private def checkBeginStatement(statements: List[Statement]) = {
        scoper.enterScope()
        if (!statements.forall(checkStatement)) false
        scoper.exitScope()
        true
    }

    private def lookupVar(v: String): Option[SAType] = {
        val iter = scoper.getIterator()
        while (iter.hasNext) {
            val key = (v, iter.next())
            if (symbolTable.contains(key)) Some(symbolTable(key))
        }
        None
    }

    private def insertVar(v: String, t: SAType): Boolean = {
        val key = (v, scoper.getScope())
        if (symbolTable.contains(key)) false
        symbolTable + (key -> t)
        true
    }

    // private def insertFunction(f: String, params: (SAType, List[SAType])) {
    //     if (functionTable.contains(f)) false
    //     functionTable + (key -> params)
    //     true
    // }

    // private def retrieveFunction(f: String) = functionTable.getOrElse(f, (Nothing, Nil))
}