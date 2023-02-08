package wacc

object Analyser {
    import wacc.AST._
    import wacc.Types._
    import wacc.TypeConverter.convertSyntaxToTypeSys
    import wacc.Keywords.keywords

    val scoper = new Scoper()
    val functionTable = Map[String, (SAType, List[SAType])]()
    var symbolTable = SymbolTable(scoper)
    val errorList = List[String]()

    private def checkUnOp(op: UnaryOp, expression: Expression): Option[SAType] = {
        op match {
            case Chr | Negation => if (checkExpression(expression, SAIntType)) Some(SAIntType) else None
            case Len => expression match {
                case Identifier(id) => symbolTable.lookupVar(id) match {
                    case Some(SAArrayType(_, _)) => Some(SAIntType) 
                    case default => None
                }
                case ArrayElem(id, indices) => symbolTable.lookupVar(id.name) match {
                    case Some(SAArrayType(_, arity)) => if (indices.length < arity) Some(SAIntType) else None
                    case default => None
                }
                case default => None
            }
            case Not => if (checkExpression(expression, SABoolType)) Some(SABoolType) else None
            case Ord => if (checkExpression(expression, SACharType)) Some(SACharType) else None
        }
    }

    private def checkBinOp(op: BinaryOp, lhs: Expression, rhs: Expression): Option[SAType] = op match {
        case Mul | Div | Mod | Plus | Minus => if (checkExpression(lhs, SAIntType) && checkExpression(rhs, SAIntType)) Some(SAIntType) else None
        case And | Or => if (checkExpression(lhs, SABoolType) && checkExpression(rhs, SABoolType)) Some(SABoolType) else None
        case Gt | Ge | Lt | Le => {bothTypesMatch(lhs, rhs, List(SAIntType, SACharType)) match {
            case Some(_) => Some(SABoolType)
            case default => None
        }
    }
        case Eq | Neq => {
            val btmRes = bothTypesMatch(lhs, rhs, List(SAIntType, SACharType, SABoolType, SAStringType)) 
            if (btmRes.isDefined) Some(SABoolType)
            else {
                // at this point, they must be composites, so they can only be from var lookups
                val lhsVar = getVarName(lhs)
                val rhsVar = getVarName(rhs)
                if (lhsVar.isDefined && rhsVar.isDefined) {
                    val lhsLookup = symbolTable.lookupVar(lhsVar.get)
                    val rhsLookup = symbolTable.lookupVar(rhsVar.get)
                    if (lhsLookup.isDefined && rhsLookup.isDefined 
                    && equalsType(lhsLookup.get, rhsLookup.get)) lhsLookup
                }
                None
            }
        }
        case default => None
    }
    

    private def getVarName(expression: Expression): Option[String] = expression match {
        case Identifier(str) => Some(str)
        case _ => None
    }

    private def bothTypesMatch(lhs: Expression, rhs: Expression, validTypes: List[SAType]): Option[SAType] = {
        for (saType <- validTypes) {
            if (checkExpression(lhs, saType) && checkExpression(rhs, saType)) return Some(saType)
        }
        return None
    }

    private def checkExpression(expression: Expression, t: SAType): Boolean = {
        expression match {
            case IntLiteral(_) => equalsType(t, SAIntType)
            case BoolLiteral(_) => equalsType(t, SABoolType)
            case CharLiteral(_) => equalsType(t, SACharType)
            case StringLiteral(_) => equalsType(t, SAStringType)
            // pairliteral logic is bs
            case PairLiteral => t match {
                case SAPairType(_, _) => true
                case default => false
            }
            case Identifier(id) => symbolTable.lookupVar(id).isDefined && equalsType(t, symbolTable.lookupVar(id).get)
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(typeName) => equalsType(t, typeName)
                case None => false
            }
            case UnaryOpApp(op, expr) => checkUnOp(op, expr) match {
                case Some(exprType) => equalsType(exprType, t)
                case _ => false
            }
            case BinaryOpApp(op, lhs, rhs) => checkBinOp(op, lhs, rhs) match {
                case Some(exprType) => equalsType(exprType, t)
                case _ => false
            }
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

    private def checkDeclarationStatement(typeName: SAType, identifier: Identifier, rvalue: RValue): Boolean = {
        val idenIsKeyword = keywords.contains(identifier.name)
        if (idenIsKeyword) {
            ("Identifier " + identifier.name + " is a WACC keyword") :: errorList
            return false
        }
        val idenNotInSymTable = symbolTable.insertVar(identifier.name, typeName)
        if (idenNotInSymTable) {
            if (!checkRValue(rvalue, typeName)) {
                ("RHS of declaration statment is not of type " + typeName) :: errorList
            } else {
                return true
            }
        } else {
            ("Identifier " + identifier.name + " already declared in the current scope") :: errorList
        }
        return true
    }
    
    private def checkRValue(rvalue: RValue, typeName: SAType): Boolean = {
        rvalue match {
            case NewPair(fst, snd) => typeName match {
                case SAPairType(fstType, sndType) => checkExpression(fst, fstType) && checkExpression(snd, sndType)
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
            case Identifier(id) => symbolTable.lookupVar(id) match {
                case Some(SAPairType(fstType, sndType)) => index match {
                    case Fst => equalsType(fstType, typeName)
                    case Snd => equalsType(sndType, typeName)
                }
                case default => false
            }
            case PairElem(anotherIndex, anotherPair) => index match {
                case Fst => checkPairElem(anotherIndex, anotherPair, SAPairType(typeName, SAAnyType))
                case Snd => checkPairElem(anotherIndex, anotherPair, SAPairType(SAAnyType, typeName))
            }
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(SAPairType(fstType, sndType)) => index match {
                    case Fst => equalsType(fstType, typeName)
                    case Snd => equalsType(sndType, typeName)
                }
                case _ => false
            }
        }
        true
    }

    private def checkAssignmentStatement(lvalue: LValue, rvalue: RValue): Boolean = {
        val typeName = lvalue match {
            case Identifier(id) => symbolTable.lookupVar(id)
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
            case Identifier(id) => symbolTable.lookupVar(id) match {
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
            case Identifier(id) => symbolTable.lookupVar(id) match {
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
        symbolTable.lookupVar(id.name) match {
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

    private def isValidExpression(expression: Expression): Boolean = checkExpression(expression, SAAnyType)
    
    // TODO: change AST node to include print type info with inferType
    // need to check whether pairelem, arrayelem, var is either int or char
    private def checkReadStatement(lvalue: LValue): Boolean =
        checkLValue(lvalue, SAIntType) || checkLValue(lvalue, SACharType)
    
    private def checkFreeStatement(expression: Expression): Boolean = 
        isExpressionArrayType(expression) || isExpressionPairType(expression)
    
    private def isExpressionArrayType(expression: Expression): Boolean = {
        val typeName = expression match {
            case Identifier(id) => symbolTable.lookupVar(id) 
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
        }
        typeName match {
            case Some(SAArrayType(_, _)) => true
            case default => false
        }
    }

    private def isExpressionPairType(expression: Expression): Boolean = {
        val typeName = expression match {
            case Identifier(id) => symbolTable.lookupVar(id)
            case PairLiteral => return true
            case ArrayElem(id, indices) => getArrayElemType(id, indices) 
        }
        typeName match {
            case Some(SAPairType(_, _)) => true
            case default => false
        }
    }

    private def checkWhileStatement(condition: Expression, doStatements: List[Statement]): Boolean = {
        if (!checkExpression(condition, SABoolType)) return false
        scoper.enterScope()
        if (!doStatements.forall(checkStatement)) return false
        scoper.exitScope()
        return true
    }

    private def checkIfStatement(condition: Expression, thenStatements: List[Statement], elseStatements: List[Statement]): Boolean = {
    if (!checkExpression(condition, SABoolType)) return false
        scoper.enterScope()
        if (!thenStatements.forall(checkStatement)) return false
        scoper.exitScope()
        scoper.enterScope()
        if (!elseStatements.forall(checkStatement)) return false
        scoper.exitScope()
        true
    }

    private def checkBeginStatement(statements: List[Statement]) = {
        scoper.enterScope()
        if (!statements.forall(checkStatement)) false
        scoper.exitScope()
        true
    }

    // private def insertFunction(f: String, params: (SAType, List[SAType])) {
    //     if (functionTable.contains(f)) false
    //     functionTable + (key -> params)
    //     true
    // }

    // private def retrieveFunction(f: String) = functionTable.getOrElse(f, (Nothing, Nil))
}