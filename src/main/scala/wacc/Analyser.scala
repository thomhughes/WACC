package wacc

object Analyser {
    import wacc.AST._
    import wacc.Types._
    import wacc.Keywords.keywords

    val scoper = new Scoper()
    val functionTable = new FunctionTable()
    var symbolTable = SymbolTable(scoper)
    val errorList = List[String]()

    private def getUnOpType(op: UnaryOp, expression: Expression): Option[SAType] = {
        op match {
            case Negation => if (checkExpression(expression, SAIntType)) Some(SAIntType) else None
            case Chr => if (checkExpression(expression, SAIntType)) Some(SACharType) else None
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
            case Ord => if (checkExpression(expression, SACharType)) Some(SAIntType) else None
        }
    }

    private def getBinOpType(op: BinaryOp, lhs: Expression, rhs: Expression): Option[SAType] = op match {
        case Mul | Div | Mod | Plus | Minus => if (checkExpression(lhs, SAIntType) && checkExpression(rhs, SAIntType)) Some(SAIntType) else None
        case And | Or => if (checkExpression(lhs, SABoolType) && checkExpression(rhs, SABoolType)) Some(SABoolType) else None
        case Gt | Ge | Lt | Le => {bothTypesMatch(lhs, rhs, List(SAIntType, SACharType)) match {
            case Some(_) => Some(SABoolType)
            case default => None
        }}
        case Eq | Neq => {
            val btmRes = bothTypesMatch(lhs, rhs, List(SAIntType, SACharType, SABoolType, SAStringType)) 
            if (btmRes.isDefined) Some(SABoolType)
            else {
                // at this point, they must be composites, so they can only be from var lookups
                getExpressionType(lhs) match {
                    case Some(lhsType) => getExpressionType(rhs) match {
                        case Some(rhsType) => if (equalsType(lhsType, rhsType)) Some(SABoolType) else None
                        case None => None
                    }
                    case None => None
                }
            }
        }
        case default => None
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
                case SAAnyType => true
                case SAPairRefType => true
                case default => false
            }
            case Identifier(id) => {
                val lookupResult = symbolTable.lookupVar(id)
                lookupResult.isDefined && equalsType(t, lookupResult.get)
            }
            case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
                case Some(typeName) => equalsType(t, typeName)
                case None => false
            }
            case UnaryOpApp(op, expr) => getUnOpType(op, expr) match {
                case Some(exprType) => equalsType(exprType, t)
                case _ => false
            }
            case BinaryOpApp(op, lhs, rhs) => getBinOpType(op, lhs, rhs) match {
                case Some(exprType) => equalsType(exprType, t)
                case _ => false
            }
            case default => false
        }
    }

    def checkProgram(program: Program) =
        checkFunctions(program) && program.statements.forall(checkStatement)

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
        val idenNotInSymTable = symbolTable.insertVar(identifier.name, typeName)
        if (!idenNotInSymTable) {
            ("Identifier " + identifier.name + " already declared in the current scope") :: errorList
            return false
        }
        if (!checkRValue(rvalue, typeName)) {
            return false
        }
        return true
    }

    private def checkArrayConstraints(list: List[Expression], expectedType: SAType, expectedArity: Int): Boolean = {
        list match {
            case Identifier(id) :: next => symbolTable.lookupVar(id) match {
                case Some(SAArrayType(actualType, innerArity)) => actualType == expectedType && (innerArity + 1) == expectedArity && checkArrayConstraints(next, expectedType, expectedArity)
                case default => false
            }
            case _ :: next => false
            case Nil => true
        }
    }
    
    private def checkRValue(rvalue: RValue, typeName: SAType): Boolean = {
        rvalue match {
            case NewPair(fst, snd) => typeName match {
                case SAPairType(fstType, sndType) => checkExpression(fst, fstType) && checkExpression(snd, sndType)
                case default => false
            }
            case ArrayLiteral(list) => typeName match {
                case SAArrayType(arrayType: SAType, 1) => list.forall((x) => checkExpression(x, arrayType))
                case SAArrayType(arrayType: SAType, x) => checkArrayConstraints(list, arrayType, x)
            }
            case PairElem(index, pair) => checkPairElem(index, pair, typeName)
            // TODO
            case FunctionCall(id, args) => checkFunctionCall(id, args, typeName)
            case expr:Expression => checkExpression(expr, typeName)
            case default => false
        }
    }

    private def checkPairElem(index: PairIndex, pair: LValue, typeName: SAType): Boolean =
        getPairElemType(index, pair) match {
            case Some(pairElemType) => equalsType(pairElemType, typeName)
            case default => false
        }

    private def checkAssignmentStatement(lvalue: LValue, rvalue: RValue): Boolean = {
        val typeName = lvalue match {
            case Identifier(id) => symbolTable.lookupVar(id)
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
            case PairElem(index, pair) => getPairElemType(index, pair)
        }
        typeName match {
            case Some(typeN) => checkRValue(rvalue, typeN)
            case default => false
        }
    }

    private def getPairElemType(index: PairIndex, pair: LValue): Option[SAType] = {
        val typeName = pair match {
            case Identifier(id) => symbolTable.lookupVar(id) 
            case PairElem(anotherIndex, anotherPair) => getPairElemType(anotherIndex, anotherPair)
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
        }
        typeName match {
            case Some(SAPairType(fstType, sndType)) => index match {
                case Fst => Some(fstType)
                case Snd => Some(sndType)
            }
            case Some(SAPairRefType) => Some(SAUnknownType)
            case default => None
        }
    }

    // private def getPairElemType(index: PairIndex, pair: LValue): Option[SAType] = {
    //     pair match {
    //         case Identifier(id) => symbolTable.lookupVar(id) match {
    //             case Some(SAPairType(fstType, sndType)) => index match {
    //                 case Fst => Some(fstType)
    //                 case Snd => Some(sndType)
    //             }
    //             case default => None
    //         }
    //         case PairElem(anotherIndex, anotherPair) => index match {
    //             case Fst => getPairElemType(anotherIndex, anotherPair) match {
    //                 case Some(SAPairType(fstType, _)) => Some(fstType)
    //                 case default => None
    //             }
    //             case Snd => getPairElemType(anotherIndex, anotherPair) match {
    //                 case Some(SAPairType(_, sndType)) => Some(sndType)
    //                 case default => None
    //             }
    //         }
    //         case ArrayElem(id, indices) => getArrayElemType(id, indices) match {
    //             case Some(SAPairType(fstType, sndType)) => index match {
    //                 case Fst => Some(fstType)
    //                 case Snd => Some(sndType)
    //             }
    //             case _ => None
    //         }
    //     }
    // }

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
            case _ => return false
        }
        typeName match {
            case Some(SAArrayType(_, _)) => true
            case _ => false
        }
    }

    private def isExpressionPairType(expression: Expression): Boolean = {
        val typeName = expression match {
            case Identifier(id) => symbolTable.lookupVar(id)
            case PairLiteral => return true
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
            case _ => return false
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

    private def checkFunctions(program: Program): Boolean = program.functions.forall(mapDefs) && program.functions.forall(checkFunction)

    private def mapDefs(function: Func): Boolean = {
        val funcName = function.identBinding.identifier.name
        val retType = convertSyntaxToTypeSys(function.identBinding.typeName)
        val params = function.params.map(_.typeName).map(convertSyntaxToTypeSys)
        return functionTable.insertFunction(funcName, (retType, params))
    }

    // TODO: implement checkFunction
    private def checkFunction(func: Func): Boolean = {
        scoper.enterScope()
        // add all params to symbol table, now in scope
        val params = func.params
        if (!params.forall(p => 
            symbolTable.insertVar(p.identifier.name, convertSyntaxToTypeSys(p.typeName)))) return false
        scoper.enterScope()
        // must be a return as last statement. We explicity test for this as there is
        // no situation where this should happen otherwise
        val funcName = func.identBinding.identifier.name
        if (!(func.body.forall(s => checkFunctionStatements(s, funcName)))) return false
        scoper.exitScope()
        scoper.exitScope()
        true
    }

    private def checkFunctionStatements(statement: Statement, funcName: String): Boolean = {
        val returnType = functionTable.getFunctionRet(funcName).get
        statement match {
            case ReturnStatement(expression) => checkExpression(expression, returnType)
            case (IfStatement(expression, s1, s2)) => {
                checkExpression(expression, SABoolType) &&
                s1.forall(s => checkFunctionStatements(s, funcName)) &&
                s2.forall(s => checkFunctionStatements(s, funcName))
            }
            case WhileStatement(expression, s) => {
                checkExpression(expression, SABoolType) && 
                s.forall(s => checkFunctionStatements(s, funcName))
            }
            case default => checkStatement(statement)
        }
    }

    private def checkFunctionCall(id: Identifier, args: List[Expression], typeName: SAType): Boolean = {
        if (!functionTable.containsFunction(id.name)) return false
        else {
            val expectedTypes = functionTable.getFunctionParams(id.name).get
            if (args.length != expectedTypes.length) return false
            var paramsMatch = true
            for (i <- 0 until args.length) if (!checkExpression(args(i), expectedTypes(i))) return false
            val returnType = functionTable.getFunctionRet(id.name)
            if (!returnType.isDefined) return false
            return equalsType(returnType.get, typeName)
        }
    }
    
    // private def getRValueType(rValue: RValue): Option[SAType] = {
    //     rValue match {
    //         case expr:Expression => getExpressionType(expr)
    //         case PairElem(pairIndex, pair) => getPairElemType(pairIndex, pair)
    //         case ArrayLiteral(exprs) => getArrayLiteralType(exprs)
    //         case NewPair(fst, snd) => getNewPairType(fst, snd)
    //         case FunctionCall(identifier, exprs) => ???
    //     }
    // }

    private def getArrayLiteralType(exprs: List[Expression]): Option[SAType] = {
        if (exprs.isEmpty) return None
        val firstType = getExpressionType(exprs.head)
        if (firstType.isEmpty) return None
        if (exprs.tail.forall(expr => getExpressionType(expr) == firstType)) Some(SAArrayType(firstType.get, 1)) else None
    }

    private def getNewPairType(fstExpr: Expression, sndExpr: Expression): Option[SAType] =
        getExpressionType(fstExpr) match {
            case Some(fstType) => getExpressionType(sndExpr) match {
                case Some(sndType) => Some(SAPairType(fstType, sndType))
                case None => None
            }
            case None => None
        }

    private def getExpressionType(expression: Expression): Option[SAType] =
        expression match {
            case IntLiteral(_) => Some(SAIntType)
            case BoolLiteral(_) => Some(SABoolType)
            case CharLiteral(_) => Some(SACharType)
            case StringLiteral(_) => Some(SAStringType)
            case PairLiteral => Some(SAPairRefType)
            case ArrayElem(id, indices) => getArrayElemType(id, indices)
            case Identifier(id) => symbolTable.lookupVar(id)
            case UnaryOpApp(op, expr) => getUnOpType(op, expr)
            case BinaryOpApp(op, lhs, rhs) => getBinOpType(op, lhs, rhs)
        }

    def convertSyntaxToTypeSys(lhsType: ASTType): SAType =
        lhsType match {
            case IntType => SAIntType
            case BoolType => SABoolType
            case CharType => SACharType
            case StringType => SAStringType
            case ArrayType(arrayType, arity) => SAArrayType(convertSyntaxToTypeSys(arrayType), arity)
            case PairType(fstType, sndType) => SAPairType(convertSyntaxToTypeSys(fstType), convertSyntaxToTypeSys(sndType))
            case PairRefType => SAPairRefType
        }
}