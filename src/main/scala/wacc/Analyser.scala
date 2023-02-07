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

    private def checkUnOp(op: UnaryOp, expression: Expression) = {
        op match {
            case Chr => checkExpression(expression, SAIntType)
            case Len => checkExpression(expression, SAArrayType(Any))
            case Negation => checkExpression(expression, SAIntType)
            case Not => checkExpression(expression, SABoolType)
            case Ord => checkExpression()
        }
    }

    private def checkBinOp(op: BinaryOp, lhs: Expression, rhs: Expression): Boolean = {
        checkExpression(lhs, SAIntType) && checkExpression(rhs, SAIntType)
    }
    
    private def ArityFinder(arr: SAType, counter: Int): (Int, SAType) = {
        arr match {
            case SAArrayType(inner) => ArityFinder(inner, counter + 1)
            case default => (counter, arr)
        }
    }

    private def checkExpression(expression: Expression, t: SAType): Boolean = {
        expression match {
            case IntLiteral(value) => t == SAIntType
            case BoolLiteral(value) => t == SABoolType
            case CharLiteral(value) => t == SACharType
            case StringLiteral(value) => t == SAStringType
            case ArrayElem(name, indices) => {
                val SAArrayType(inner) = t
                
            }
            // case PairLiteral => 
            case UnaryOpApp(op, expr) => checkUnOp(op, expr)
            case BinaryOpApp(op, lhs, rhs) => checkBinOp(op, lhs, rhs)
            case default => false
        }
    }

    // TODO: maybe simplify control flow
    private def checkProgram(program: Program) =
        program.functions.forall(checkFunction) && program.statements.forall(checkStatement)
    
    // TODO: implement checkFunction
    private def checkFunction(func: Function) = true

    private def checkStatement(statement: Statement) = {
        statement match {
            case SkipStatement => true
            case DeclarationStatement(typeName, identifier, rvalue) => checkDeclarationStatement(typeName, identifier, rvalue)
            case AssignmentStatement(lvalue, rvalue) => checkAssignmentStatement(lvalue, rvalue)
            // case ReadStatement(lvalue) => checkReadStatement(lvalue)
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

    private def checkDeclarationStatement(typeName: Type, identifier: Identifier, rvalue: RValue) = {
        val idenIsKeyword = keywords.contains(identifier.name)
        if (idenIsKeyword) {
            ("Identifier " + identifier.name + " is a WACC keyword") :: errorList
        }
        val idenNotInSymTable = insertVar(identifier.name, convertSyntaxToTypeSys(typeName))
        if (idenNotInSymTable) {
            if (checkRValue(rvalue, typeName)) {
                ("RHS of declaration statment is not of type " + typeName) :: errorList
            } else {
                return true
            }
        } else {
            ("Identifier " + identifier.name + " already declared in the current scope") :: errorList
        }
        false
    }

    private def inferType(expression: Expression): SAType = {
        expression match {
            case ArrayElem => 
            case ArrayLiteral =>
            case CharLiteral => SACharType
            case BinaryOpApp => 
            case BoolLiteral => SABoolType
            case Identifier => 
            case IntLiteral => SAIntType
            case PairLiteral => 
            case StringLiteral => SAStringType
            case UnaryOpApp(unaryOp: UnaryOp, uExpression: Expression) => inferType(uExpression) 
        }
    }
    
    private def checkRValue(rvalue: RValue, typeName: SAType): Boolean = {
        rvalue match {
            case NewPair(fst, snd) => typeName match {
                case SAPairType(fstType, sndType) => fstType == fst && snd == sndType
                case default => false
            }
            case PairElem(index, lvalue) => // (recursive) symbol table lookup of index(lvalue) to get type
            case Identifier(id) => // symbol table lookup to get type
            case IntLiteral(_) => typeName == SAIntType
            case ArrayLiteral(list) => // look up elements of array in symbol table to get type
            case StringLiteral => typeName == SAStringType
            case UnaryOpApp => 
            case ArrayElem => 
            case CharLiteral => 
            case BinaryOpApp => 
            case BoolLiteral => 
            case FunctionCall => 
            case default => false
        }
    }

    private def checkAssignmentStatement(lvalue: LValue, rvalue: RValue) = {
        rvalue match {
            case NewPair(fst, snd) => lvalue match {
                case PairElem(index, lvalue) => 
                case Identifier(id) => 
                case default => false
            }
            case PairElem(index, lvalue) =>
            case Identifier(id) => checkLValue(lavlue, lookupVar(id))
            case IntLiteral(_) => checkLValue(lvalue, SAIntType)
            case ArrayLiteral(list) => inferType(list.head) match {
                case SAArrayType(innerType) => list.forall(checkExpression(_, innerType))
                case default => false
            }
            case StringLiteral(_) => checkLValue(lvalue, SAStringType)
            case UnaryOpApp(op, expr) => 
            case ArrayElem(id, indices) =>
            case CharLiteral(_) => checkLValue(lvalue, SACharType)
            case BinaryOpApp(op, lhs, rhs) =>
            case BoolLiteral(_) => checkLValue(lvalue, SABoolType)
            case FunctionCall(id, args) => 
            case default => false
        }
    }

    private def checkLValue(lvalue: LValue, typeName: SAType): Boolean = {
        lvalue match {
            case PairElem(index, innerLValue) => checkLValue()
            case Identifier(id) => lookupVar(id) == typeName
            case ArrayElem(identifier, indices) =>  
            case default => false
        }
    }

    private def checkExitStatement(expression: Expression) = checkExpression(expression, SAIntType)

    // could optimise here and read statement
    private def checkPrintStatement(expression : Expression) = {
        // inferType(expression, SAIntType)
    }

    private def checkPrintLnStatement(expression: Expression) = {
        // checkExpression(expression, SAType)s
    }
    
    private def checkFreeStatement(expression: Expression) = {
        // checkExpression (expression, SAArray) || checkExpression(expression, SAPair)
    }

    private def checkWhileStatement(condition: Expression, doStatements: List[Statement]) = {
        if (!checkExpression(condition, SABool)) {
            return false
        }
        scoper.enterScope()
        if (!doStatements.forall(checkStatement)) {
            return false
        }
        scoper.exitScope()
        return true
    }

    private def checkIfStatement(condition: Expression, thenStatements: List[Statement], elseStatements: List[Statement]) = {
        // checkExpression(condition, SABool)
        scoper.enterScope()
        val successThen = thenStatements.forall(checkStatement)
        if (!successThen) {
            return false
        }
        scoper.exitScope()
        scoper.enterScope()
        val successElse = elseStatements.forall(checkStatement)
        if (!successElse) {
            return false
        }
        scoper.exitScope()
    }

    private def checkBeginStatement(statements: List[Statement]) = {
        scoper.enterScope()
        val success = statements.forall(checkStatement)
        if (!success) {
            return false
        }
        scoper.exitScope()
    }

    private def varExists(v: String, t: SAType): Boolean = {
        val iter = scoper.getIterator()
        
        while (iter.hasNext) {
            var key = (v, iter.next())
            if (symbolTable.contains(key)) {
                return symbolTable(key) == t
            }
        }
        return false
    }

    private def findVar(v: String); SAType = {
        // TODO
    }

    private def insertVar(v: String, t: SAType): Boolean = {
        val key = (v, scoper.getScope())

        if (symbolTable.contains(key)) {
            return false
        }
        symbolTable + (key -> t)
        return true
    }

    private def insertFunction(f: String, params: (SAType, List[SAType])) {
        if (functionTable.contains(f)) {
            return false
        }
        functionTable + (key -> params)
        return true
    }

    private def retrieveFunction(f: String) = functionTable.getOrElse(f, (Nothing, Nil))
}