package wacc

object Analyser {
  import wacc.AST._
  import wacc.Types._
  import wacc.Errors._

  val scoper = new Scoper()
  val functionTable = new FunctionTable()
  var symbolTable = SymbolTable(scoper)
  implicit val returnVal: SAType = SAAnyType

  // HELPERS
  private def convertSyntaxToTypeSys(lhsType: ASTType): SAType = lhsType match {
    case IntType    => SAIntType
    case BoolType   => SABoolType
    case CharType   => SACharType
    case StringType => SAStringType
    case ArrayType(arrayType, arity) =>
      SAArrayType(convertSyntaxToTypeSys(arrayType), arity)
    case PairType(fstType, sndType) =>
      SAPairType(convertSyntaxToTypeSys(fstType),
                 convertSyntaxToTypeSys(sndType))
    case PairRefType => SAPairRefType
    case default     => throw new Exception("Unknown type")
  }

  private def getTypeIfExpressionsType(expressions: List[Expression],
                                       t: SAType,
                                       o: SAType)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    expressions match {
      case expression :: tail => {
        val typeError = checkExpression(expression, t)
        if (!(typeError eq errorList)) {
          return Right(typeError)
        }
        getTypeIfExpressionsType(tail, t, o)
      }
      case Nil => Left(o)
    }
  }

  // Function to wrap "equalsType" and return error[List]
  private def equalsTypeNoError(
      pos: Position,
      firstType: SAType,
      secondType: SAType)(implicit errorList: List[Error]): List[Error] =
    equalsType(firstType, secondType) match {
      case true => {
        errorList
      }
      case default => {
        errorList :+ TypeError(pos,
                               firstType.toString,
                               List(secondType.toString))
      }
    }

  // Position helpers

  private def getExpressionPos(expression: Expression): Position =
    expression match {
      case e @ IntLiteral(_)        => e.pos
      case e @ BoolLiteral(_)       => e.pos
      case e @ CharLiteral(_)       => e.pos
      case e @ StringLiteral(_)     => e.pos
      case e @ PairLiteral          => (0, 0)
      case e @ UnaryOpApp(_, _)     => e.pos
      case e @ BinaryOpApp(_, _, _) => e.pos
      case e @ ArrayElem(_, _)      => e.pos
      case e @ Identifier(_)        => e.pos
    }

  private def getLValuePos(lvalue: LValue): Position = lvalue match {
    case id @ Identifier(_)          => id.pos
    case pairelem @ PairElem(_, _)   => pairelem.pos
    case arrayelem @ ArrayElem(_, _) => arrayelem.pos
  }

  private def getRValuePos(rvalue: RValue): Position = rvalue match {
    case np @ NewPair(fst, snd)      => np.pos
    case l @ ArrayLiteral(list)      => l.pos
    case pe @ PairElem(index, pair)  => pe.pos
    case fc @ FunctionCall(id, args) => fc.pos
    case expr: Expression            => getExpressionPos(expr)
  }

  private def collectErrors[A](a: List[A], collector: A => List[Error])(
      implicit errorList: List[Error]): List[Error] = a match {
    case head :: b => collectErrors(b, collector)(errorList :++ collector(head))
    case Nil       => errorList
  }

  private def collectErrors2[A, B](a: List[(A, B)],
                                   collector: (A, B) => List[Error])(
      implicit errorList: List[Error]): List[Error] = {
    a match {
      case (a1, b1) :: c =>
        collectErrors2(c, collector)(errorList :++ collector(a1, b1))
      case Nil => errorList
    }
  }

  private def getLValueName(lvalue: LValue): String = lvalue match {
    case Identifier(name)         => name
    case PairElem(_, value)       => getLValueName(value)
    case ArrayElem(identifier, _) => identifier.name
  }

  private def isExpressionArrayOrPairType(expression: Expression)(
      implicit errorList: List[Error]): List[Error] = {
    val typeName = expression match {
      case id @ Identifier(_)     => symbolTable.lookupVar(id)
      case PairLiteral            => return errorList
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case _                      => getExpressionType(expression)
    }
    typeName match {
      case Left(SAArrayType(_, _)) => errorList
      case Left(SAPairType(_, _))  => errorList
      case Left(t) =>
        errorList :+ TypeError(getExpressionPos(expression),
                               t.toString,
                               List(SAPairType.toString, SAArrayType.toString))
      case Right(el) => el
      case default   => throw new Exception("This shouldnt happen either.")
    }
  }

  private def checkArrayConstraints(
      rvalue: RValue,
      list: List[Expression],
      expectedType: SAType,
      expectedArity: Int)(implicit errorList: List[Error]): List[Error] = {
    list match {
      case (id @ Identifier(_)) :: next =>
        symbolTable.lookupVar(id) match {
          case Left(SAArrayType(actualType, innerArity)) => {
            val equalsTypeErrors =
              equalsTypeNoError(getRValuePos(rvalue), actualType, expectedType)
            if (!(equalsTypeErrors eq errorList)) {
              return equalsTypeErrors
            } else if ((innerArity + 1) != expectedArity) {
              return (errorList :+ TypeError(
                getRValuePos(rvalue),
                SAArrayType(actualType, innerArity + 1).toString,
                List(SAArrayType(expectedType, expectedArity).toString)))
            }
            return checkArrayConstraints(rvalue,
                                         next,
                                         expectedType,
                                         expectedArity)
          }
          case Left(t) =>
            errorList :+ TypeError(
              getRValuePos(rvalue),
              t.toString,
              List(SAArrayType(expectedType, expectedArity).toString))
          case Right(el) => el
        }
      case (e: Expression) :: next =>
        getExpressionType(e) match {
          case Left(expType) =>
            errorList :+ TypeError(
              getRValuePos(rvalue),
              expType.toString,
              List(SAArrayType(expectedType, expectedArity).toString))
          case _ => isValidExpression(e)
        }
      case Nil => return errorList
    }
  }

  private def checkRValue(rvalue: RValue, typeName: SAType)(
      implicit errorList: List[Error]): List[Error] = {
    rvalue match {
      case NewPair(fst, snd) =>
        typeName match {
          case SAPairType(fstType, sndType) =>
            checkExpression(snd, sndType)(checkExpression(fst, fstType))
          case default =>
            errorList :+ NewPairError(
              getRValuePos(rvalue),
              "cannot create new pair of type" + typeName)
        }
      case al @ ArrayLiteral(list) =>
        typeName match {
          case SAArrayType(arrayType: SAType, 1) =>
            collectErrors(list,
                          (x: Expression) => (checkExpression(x, arrayType)))
          case SAArrayType(arrayType: SAType, x) =>
            checkArrayConstraints(rvalue, list, arrayType, x)
          case default =>
            errorList :+ ArrayLiteralError(
              al.pos,
              "invalid array literal of type" + typeName)
        }
      case PairElem(index, pair)  => checkPairElem(index, pair, typeName)
      case FunctionCall(id, args) => checkFunctionCall(id, args, typeName)
      case expr: Expression       => checkExpression(expr, typeName)
    }
  }

  // STATEMENT RELATED FUNCTIONS
  private def checkDeclarationStatement(
      typeName: SAType,
      identifier: Identifier,
      rvalue: RValue)(implicit errorList: List[Error]): List[Error] = {
    return checkRValue(rvalue, typeName)(
      symbolTable.insertVar(identifier, typeName))
  }

  private def checkAssignmentStatement(lvalue: LValue, rvalue: RValue)(
      implicit errorList: List[Error]): List[Error] = {
    val typeName = lvalue match {
      case id @ Identifier(_)     => symbolTable.lookupVar(id)
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case PairElem(index, pair)  => getPairElemType(index, pair)
      case default                => throw new Exception("Unresolved Syntax Error")
    }
    typeName match {
      case Left(typeN) => checkRValue(rvalue, typeN)
      case Right(el)   => el
    }
  }

  private def getPairElemType(index: PairIndex, pair: LValue)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = {
    val typeName = pair match {
      case id @ Identifier(_) => symbolTable.lookupVar(id)
      case PairElem(anotherIndex, anotherPair) =>
        getPairElemType(anotherIndex, anotherPair)
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case default                => throw new Exception("Exhaustive")
    }
    typeName match {
      case Left(SAPairType(fstType, sndType)) =>
        index match {
          case Fst => Left(fstType)
          case Snd => Left(sndType)
        }
      case Left(SAPairRefType) => Left(SAUnknownType)
      case Left(t) =>
        Right(
          errorList :+ TypeError(getLValuePos(pair),
                                 t.toString,
                                 List(SAPairType.toString,
                                      SAPairRefType.toString)))
      case rel @ Right(_) => rel
    }
  }

  private def checkPairElem(index: PairIndex, pair: LValue, typeName: SAType)(
      implicit errorList: List[Error]): List[Error] =
    getPairElemType(index, pair) match {
      case Left(pairElemType) =>
        equalsTypeNoError(getLValuePos(pair), pairElemType, typeName)
      case Right(errorList) => errorList
    }

  // TODO: change AST node to include print type info with inferType
  private def checkReadStatement(lvalue: LValue)(
      implicit errorList: List[Error]): List[Error] = {
    val charError = checkLValue(lvalue, SACharType)
    val intError = checkLValue(lvalue, SAIntType)(charError)
    if ((!(charError eq errorList) && !(intError eq charError))) {
      intError :+ ReadStatementError(getLValuePos(lvalue))
    } else {
      errorList
    }
  }

  private def checkFreeStatement(expression: Expression)(
      implicit errorList: List[Error]): List[Error] =
    isExpressionArrayOrPairType(expression)

  private def checkExitStatement(expression: Expression)(
      implicit errorList: List[Error]) = checkExpression(expression, SAIntType)

  // TODO: change AST node to include print type info with inferType
  private def checkPrintStatement(expression: Expression)(
      implicit errorList: List[Error]) = isValidExpression(expression)

  // TODO: change AST node to include print type info with inferType
  private def checkPrintLnStatement(expression: Expression)(
      implicit errorList: List[Error]) = isValidExpression(expression)

  private def checkIfStatement(condition: Expression,
                               thenStatements: List[Statement],
                               elseStatements: List[Statement])(
      implicit returnVal: SAType,
      errorList: List[Error]): List[Error] = {
    val conditionError = checkExpression(condition, SABoolType)
    if (!(conditionError eq errorList)) return conditionError
    scoper.enterScope()
    val thenErrors =
      collectErrors(thenStatements, checkStatement)(conditionError)
    scoper.exitScope()
    scoper.enterScope()
    val elseErrors = collectErrors(elseStatements, checkStatement)(thenErrors)
    scoper.exitScope()
    elseErrors
  }

  private def isValidExpression(expression: Expression)(
      implicit errorList: List[Error]): List[Error] =
    checkExpression(expression, SAAnyType)

  private def checkLValue(lvalue: LValue, typeName: SAType)(
      implicit errorList: List[Error]): List[Error] = {
    lvalue match {
      case id @ Identifier(_) =>
        symbolTable.lookupVar(id) match {
          case Left(t)   => equalsTypeNoError(getLValuePos(lvalue), t, typeName)
          case Right(el) => el
        }
      case ArrayElem(id, indices) =>
        getArrayElemType(id, indices) match {
          case Left(t)   => equalsTypeNoError(getLValuePos(lvalue), t, typeName)
          case Right(el) => el
        }
      case PairElem(index, innerLValue) =>
        checkPairElem(index, innerLValue, typeName)
    }
  }

  private def checkWhileStatement(condition: Expression,
                                  doStatements: List[Statement])(
      implicit returnVal: SAType,
      errorList: List[Error]): List[Error] = {
    val conditionError = checkExpression(condition, SABoolType)
    if (!(conditionError eq errorList)) return conditionError
    scoper.enterScope()
    val statementErrors =
      collectErrors(doStatements, checkStatement)(conditionError)
    scoper.exitScope()
    statementErrors
  }

  private def checkBeginStatement(statements: List[Statement])(
      implicit returnVal: SAType,
      errorList: List[Error]): List[Error] = {
    scoper.enterScope()
    val statementErrors = collectErrors(statements, checkStatement)
    scoper.exitScope()
    statementErrors
  }

  private def checkStatement(statement: Statement)(
      implicit returnVal: SAType,
      errorList: List[Error]): List[Error] = statement match {
    case SkipStatement => errorList
    case DeclarationStatement(typeName, identifier, rvalue) =>
      checkDeclarationStatement(convertSyntaxToTypeSys(typeName),
                                identifier,
                                rvalue)
    case AssignmentStatement(lvalue, rvalue) =>
      checkAssignmentStatement(lvalue, rvalue)
    case ReadStatement(lvalue)     => checkReadStatement(lvalue)
    case FreeStatement(expression) => checkFreeStatement(expression)
    case ReturnStatement(expression) =>
      returnVal match {
        case SAAnyType =>
          errorList :+ ReturnFromMainError(getExpressionPos(expression))
        case default => checkExpression(expression, returnVal)
      }
    case ExitStatement(expression)    => checkExitStatement(expression)
    case PrintStatement(expression)   => checkPrintStatement(expression)
    case PrintLnStatement(expression) => checkPrintLnStatement(expression)
    case IfStatement(condition, thenStatements, elseStatements) =>
      checkIfStatement(condition, thenStatements, elseStatements)
    case WhileStatement(condition, doStatements) =>
      checkWhileStatement(condition, doStatements)
    case BeginStatement(statements) => checkBeginStatement(statements)
  }

  // EXPRESSION RELATED FUNCTIONS
  private def bothTypesMatch(lhs: Expression,
                             rhs: Expression,
                             validTypes: List[SAType])(
      implicit errorList: List[Error]): Either[SAType, List[Error]] =
    validTypes match {
      case t :: tail => {
        getTypeIfExpressionsType(List(lhs, rhs), t, t) match {
          case l @ Left(_) =>
            getExpressionType(lhs) match {
              case Left(ll) =>
                getExpressionType(rhs) match {
                  case Left(lr) if equalsType(ll, lr) => l
                  case Left(lr) =>
                    Right(
                      errorList :+ TypeError(getExpressionPos(lhs),
                                             ll.toString,
                                             List(lr.toString)))
                  case rel @ Right(_) => rel
                }
              case rel @ Right(_) => rel
            }
          case Right(el) => bothTypesMatch(lhs, rhs, tail)
        }
      }
      case Nil => Right(errorList)
    }

  private def getArrayElemType(id: Identifier, indices: List[Expression])(
      implicit errorList: List[Error]): Either[SAType, List[Error]] =
    symbolTable.lookupVar(id) match {
      case Left(SAArrayType(arrayType, arity)) => {
        val indicesError = collectErrors(
          indices,
          (x: Expression) => checkExpression(x, SAIntType))
        if (!(indicesError eq errorList))
          Right(indicesError)
        else if (indices.length < arity)
          Left(SAArrayType(arrayType, arity - indices.length))
        else if (indices.length == arity)
          Left(arrayType)
        else
          Right(
            indicesError :+ ArrayArityError(id.pos,
                                            arity - indices.length,
                                            arity))
      }
      case Left(t) =>
        Right(
          errorList :+ TypeError(id.pos,
                                 t.toString,
                                 List(SAArrayType.toString)))
      case rel @ Right(_) => rel
    }

  private def getUnOpType(op: UnaryOp, expression: Expression)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] = op match {
    case Negation =>
      getTypeIfExpressionsType(List(expression), SAIntType, SAIntType)
    case Chr =>
      getTypeIfExpressionsType(List(expression), SAIntType, SACharType)
    case Len =>
      expression match {
        case id @ Identifier(_) =>
          symbolTable.lookupVar(id) match {
            case Left(SAArrayType(_, _)) => Left(SAIntType)
            case rel @ Right(_)          => rel
          }
        case ArrayElem(id, indices) =>
          symbolTable.lookupVar(id) match {
            case Left(SAArrayType(_, arity)) =>
              if (indices.length < arity) {
                Left(SAIntType)
              } else
                Right(
                  errorList :+ ArrayArityError(getExpressionPos(expression),
                                               indices.length,
                                               arity))
            case rel @ Right(_) => rel
          }
        case default =>
          Right(
            errorList :+ TypeError(getExpressionPos(expression),
                                   expression.toString,
                                   List(SAArrayType.toString)))
      }
    case Not =>
      getTypeIfExpressionsType(List(expression), SABoolType, SABoolType)
    case Ord =>
      getTypeIfExpressionsType(List(expression), SACharType, SAIntType)
  }

  private def getBinOpType(binaryOp: BinaryOpApp)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] =
    binaryOp.op match {
      case Mul | Div | Mod | Plus | Minus =>
        getTypeIfExpressionsType(List(binaryOp.lhs, binaryOp.rhs),
                                 SAIntType,
                                 SAIntType)
      case And | Or =>
        getTypeIfExpressionsType(List(binaryOp.lhs, binaryOp.rhs),
                                 SABoolType,
                                 SABoolType)
      case Gt | Ge | Lt | Le => {
        bothTypesMatch(binaryOp.lhs, binaryOp.rhs, List(SAIntType, SACharType)) match {
          case Left(_) => Left(SABoolType)
          case default =>
            Right(
              errorList :+ BinaryOpAppTypeError(binaryOp.pos,
                                                List(SAIntType.toString,
                                                     SACharType.toString)))
        }
      }
      case Eq | Neq => {
        getExpressionType(binaryOp.lhs) match {
          case Left(lhsType) =>
            getExpressionType(binaryOp.rhs) match {
              case Left(rhsType) if equalsType(lhsType, rhsType) =>
                Left(SABoolType)
              case Left(rhsType) =>
                Right(
                  errorList :+ BinaryOpAppTypeError(
                    binaryOp.pos,
                    List(SAIntType, SACharType, SABoolType, SAStringType).map(
                      _.toString)))
              case Right(errorList) =>
                Right(
                  errorList :+ BinaryOpAppTypeError(
                    binaryOp.pos,
                    List(SAIntType, SACharType, SABoolType, SAStringType).map(
                      _.toString)))
            }
          case Right(errorList) =>
            Right(
              errorList :+ BinaryOpAppTypeError(
                binaryOp.pos,
                List(SAIntType, SACharType, SABoolType, SAStringType).map(
                  _.toString)))
        }
      }
    }

  private def getExpressionType(expression: Expression)(
      implicit errorList: List[Error]): Either[SAType, List[Error]] =
    expression match {
      case IntLiteral(_)              => Left(SAIntType)
      case BoolLiteral(_)             => Left(SABoolType)
      case CharLiteral(_)             => Left(SACharType)
      case StringLiteral(_)           => Left(SAStringType)
      case PairLiteral                => Left(SAPairRefType)
      case ArrayElem(id, indices)     => getArrayElemType(id, indices)
      case id @ Identifier(_)         => symbolTable.lookupVar(id)
      case UnaryOpApp(op, expr)       => getUnOpType(op, expr)
      case boa @ BinaryOpApp(_, _, _) => getBinOpType(boa)
      case default                    => throw new Exception("we shouldnt get this!")
    }

  private def checkExpression(expression: Expression, t: SAType)(
      implicit errorList: List[Error]): List[Error] =
    getExpressionType(expression) match {
      case Left(otherType) =>
        equalsTypeNoError(getExpressionPos(expression), t, otherType)
      case Right(el) => el
    }

  // FUNCTION RELATED
  private def mapDefs(function: Func)(
      implicit errorList: List[Error]): List[Error] = {
    val retType = convertSyntaxToTypeSys(function.identBinding.typeName)
    val params = function.params.map(_.typeName).map(convertSyntaxToTypeSys)
    functionTable.insertFunction(function, (retType, params))
  }

  private def checkFunctionCall(
      id: Identifier,
      args: List[Expression],
      typeName: SAType)(implicit errorList: List[Error]): List[Error] = {
    functionTable.getFunctionEntry(id) match {
      case Left((_, expectedTypes)) if expectedTypes.length != args.length =>
        errorList :+ FunctionCallError(id.pos,
                                       args.length,
                                       expectedTypes.length)
      case Left((returnType, expectedTypes)) =>
        collectErrors2(
          args.zip(expectedTypes),
          (x: Expression, y: SAType) => checkExpression(x, y)) :++ equalsTypeNoError(
          id.pos,
          typeName,
          returnType)
      case Right(el) => el
    }
  }

  private def checkFunction(func: Func)(
      implicit errorList: List[Error]): List[Error] = {
    def collectErrorsFunctionStatements(a: List[Statement], returnVal: SAType)(
        implicit errorList: List[Error]): List[Error] = {
      a match {
        case a :: b =>
          collectErrorsFunctionStatements(b, returnVal)(
            checkStatement(a)(returnVal, errorList))
        case Nil => errorList
      }
    }
    scoper.enterScope()
    // add all params to symbol table, now in scope
    val params = func.params
    val paramErrors = collectErrors(
      params,
      (p: Parameter) =>
        symbolTable.insertVar(p.identifier, convertSyntaxToTypeSys(p.typeName)))
    if (!(paramErrors eq errorList)) {
      return paramErrors
    }
    scoper.enterScope()
    val functionReturnType: SAType = functionTable.getFunctionRet(func) match {
      case Left(t) => t
      case default => SAAnyType
    }
    val funcErrors =
      collectErrorsFunctionStatements(func.body, functionReturnType)
    scoper.exitScope()
    scoper.exitScope()
    funcErrors
  }

  private def checkFunctions(program: Program)(
      implicit errorList: List[Error]): List[Error] = {
    collectErrors(program.functions, checkFunction)(
      collectErrors(program.functions, mapDefs))
  }

  // Main function to run semantic analysis, return errors as state
  def checkProgram(program: Program): List[Error] = {
    implicit val errorList: List[Error] = List()
    collectErrors(program.statements, (x: Statement) => checkStatement(x))(
      checkFunctions(program))
  }
}
