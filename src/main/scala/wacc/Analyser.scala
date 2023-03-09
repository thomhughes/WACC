package wacc

import wacc.SymbolTable
import scala.collection.mutable.ListBuffer

object Analyser {
  import wacc.AST._
  import wacc.Types._
  import wacc.Errors._
  import scala.collection.mutable.Map

  val functionTable = new FunctionTable()
  var symbolTable = SymbolTable()
  implicit val returnVal: SAType = SAAnyType

  // HELPERS
  def convertSyntaxToTypeSys(lhsType: ASTType): SAType = lhsType match {
    case IntType    => SAIntType
    case BoolType   => SABoolType
    case CharType   => SACharType
    case StringType => SAStringType
    case ArrayType(arrayType, arity) =>
      SAArrayType(convertSyntaxToTypeSys(arrayType), arity)
    case PairType(fstType, sndType) =>
      SAPairType(
        convertSyntaxToTypeSys(fstType),
        convertSyntaxToTypeSys(sndType)
      )
    case PairRefType => SAPairRefType
    case _           => throw new Exception("Unknown type")
  }

  private def getTypeIfExpressionsType(
      expressions: List[Expression],
      t: SAType,
      o: SAType
  )(implicit
    errorList: List[Error],
    funcName: String): Either[SAType, List[Error]] = {
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
      secondType: SAType
  )(implicit errorList: List[Error]): List[Error] =
    equalsType(firstType, secondType) match {
      case true => {
        errorList
      }
      case _ => {
        errorList :+ TypeError(
          pos,
          firstType.toString,
          List(secondType.toString)
        )
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
      case _                        => throw new Exception("Unexpected Expression Type")
    }

  private def getLValuePos(lvalue: LValue): Position = lvalue match {
    case id @ Identifier(_)          => id.pos
    case pairelem @ PairElem(_, _)   => pairelem.pos
    case arrayelem @ ArrayElem(_, _) => arrayelem.pos
    case _                           => throw new Exception("Unexpected LValue")
  }

  private def getRValuePos(rvalue: RValue): Position = rvalue match {
    case np @ NewPair(fst, snd)      => np.pos
    case l @ ArrayLiteral(list)      => l.pos
    case pe @ PairElem(index, pair)  => pe.pos
    case fc @ FunctionCall(id, args) => fc.pos
    case expr: Expression            => getExpressionPos(expr)
    case _                           => throw new Exception("Unexpected RValue")
  }

  private def collectErrors[A](a: List[A], collector: A => List[Error])(
      implicit
      errorList: List[Error]): List[Error] = a match {
    case head :: b => collectErrors(b, collector)(errorList :++ collector(head))
    case Nil       => errorList
  }

  private def collectErrors2[A, B](
      a: List[(A, B)],
      collector: (A, B) => List[Error]
  )(implicit errorList: List[Error]): List[Error] = {
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
    case _                        => throw new Exception("Unexpected LValue")
  }

  private def isExpressionArrayOrPairType(
      expression: Expression
  )(implicit errorList: List[Error], funcName: String): List[Error] = {
    val typeName = expression match {
      case id @ Identifier(_)     => symbolTable.lookupVarType(id)
      case PairLiteral            => return errorList
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case _                      => getExpressionType(expression)
    }
    typeName match {
      case Left(SAArrayType(_, _)) => errorList
      case Left(SAPairType(_, _))  => errorList
      case Left(t) =>
        errorList :+ TypeError(
          getExpressionPos(expression),
          t.toString,
          List(SAPairType.toString, SAArrayType.toString)
        )
      case Right(el) => el
      case _         => throw new Exception("This shouldnt happen either.")
    }
  }

  private def checkArrayConstraints(
      rvalue: RValue,
      list: List[Expression],
      expectedType: SAType,
      expectedArity: Int
  )(implicit errorList: List[Error], funcName: String): List[Error] = {
    list match {
      case (id @ Identifier(_)) :: next =>
        symbolTable.lookupVarType(id) match {
          case Left(SAArrayType(actualType, innerArity)) => {
            val equalsTypeErrors =
              equalsTypeNoError(getRValuePos(rvalue), actualType, expectedType)
            if (!(equalsTypeErrors eq errorList)) {
              return equalsTypeErrors
            } else if ((innerArity + 1) != expectedArity) {
              return (errorList :+ TypeError(
                getRValuePos(rvalue),
                SAArrayType(actualType, innerArity + 1).toString,
                List(SAArrayType(expectedType, expectedArity).toString)
              ))
            }
            return checkArrayConstraints(
              rvalue,
              next,
              expectedType,
              expectedArity
            )
          }
          case Left(t) =>
            errorList :+ TypeError(
              getRValuePos(rvalue),
              t.toString,
              List(SAArrayType(expectedType, expectedArity).toString)
            )
          case Right(el) => el
        }
      case (e: Expression) :: next =>
        getExpressionType(e) match {
          case Left(expType) =>
            errorList :+ TypeError(
              getRValuePos(rvalue),
              expType.toString,
              List(SAArrayType(expectedType, expectedArity).toString)
            )
          case _ => isValidExpression(e)
        }
      case Nil => return errorList
    }
  }

  private def checkRValue(rvalue: RValue, typeName: SAType)(
      implicit
      errorList: List[Error],
      funcName: String,
      funcMap: Map[String, String]): List[Error] = {
    rvalue match {
      case NewPair(fst, snd) =>
        typeName match {
          case SAPairType(fstType, sndType) =>
            checkExpression(snd, sndType)(
              checkExpression(fst, fstType),
              funcName
            )
          case _ =>
            errorList :+ NewPairError(
              getRValuePos(rvalue),
              "cannot create new pair of type " + typeName
            )
        }
      case al @ ArrayLiteral(list) =>
        typeName match {
          case SAArrayType(arrayType: SAType, 1) =>
            collectErrors(
              list,
              (x: Expression) => (checkExpression(x, arrayType))
            )
          case SAArrayType(arrayType: SAType, x) =>
            checkArrayConstraints(rvalue, list, arrayType, x)
          case _ =>
            errorList :+ ArrayLiteralError(
              al.pos,
              "invalid array literal of type " + typeName
            )
        }
      case PairElem(index, pair)  => checkPairElem(index, pair, typeName)
      case FunctionCall(id, args) => checkFunctionCall(id, args, typeName)
      case expr: Expression       => checkExpression(expr, typeName)
      case _                      => throw new Exception("Unexpected RValue")
    }
  }

  // STATEMENT RELATED FUNCTIONS
  private def checkDeclarationStatement(
      typeName: SAType,
      identifier: Identifier,
      rvalue: RValue
  )(implicit errorList: List[Error], funcName: String, funcMap: Map[String, String]): List[Error] = {
    return checkRValue(rvalue, typeName)(
      symbolTable.insertVar(identifier, typeName),
      funcName, funcMap
    )
  }

  private def checkAssignmentStatement(lvalue: LValue, rvalue: RValue)(
      implicit
      errorList: List[Error],
      funcName: String,
      funcMap: Map[String, String]): List[Error] = {
    val typeName = lvalue match {
      case id @ Identifier(_)     => symbolTable.lookupVarType(id)
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case PairElem(index, pair)  => getPairElemType(index, pair)
      case _                      => throw new Exception("Unresolved Syntax Error")
    }
    typeName match {
      case Left(typeN) => checkRValue(rvalue, typeN)
      case Right(el)   => el
    }
  }

  private def getLValueType(lvalue: LValue)(
      implicit
      errorList: List[Error],
      funcName: String): Either[SAType, List[Error]] = {
    lvalue match {
      case id @ Identifier(_) => {
        symbolTable.lookupVarType(id)
      }
      case PairElem(anotherIndex, anotherPair) =>
        getPairElemType(anotherIndex, anotherPair)
      case ArrayElem(id, indices) => getArrayElemType(id, indices)
      case _                      => throw new Exception("Exhaustive")
    }
  }

  private def getPairElemType(index: PairIndex, pair: LValue)(
      implicit
      errorList: List[Error],
      funcName: String): Either[SAType, List[Error]] = {
    val typeName = getLValueType(pair)
    typeName match {
      case Left(SAPairType(fstType, sndType)) =>
        index match {
          case Fst => Left(fstType)
          case Snd => Left(sndType)
        }
      case Left(SAPairRefType) => Left(SAUnknownType)
      case Left(t) =>
        Right(
          errorList :+ TypeError(
            getLValuePos(pair),
            t.toString,
            List(SAPairType.toString, SAPairRefType.toString)
          )
        )
      case rel @ Right(_) => rel
    }
  }

  private def checkPairElem(
      index: PairIndex,
      pair: LValue,
      typeName: SAType
  )(implicit errorList: List[Error], funcName: String): List[Error] =
    getPairElemType(index, pair) match {
      case Left(pairElemType) =>
        equalsTypeNoError(getLValuePos(pair), pairElemType, typeName)
      case Right(errorList) => errorList
    }

  private def checkReadStatement(
      lvalue: LValue
  )(implicit errorList: List[Error], funcName: String): List[Error] = {
    val charError = checkLValue(lvalue, SACharType)
    val intError = checkLValue(lvalue, SAIntType)(charError, funcName)
    if ((!(charError eq errorList) && !(intError eq charError))) {
      intError :+ ReadStatementError(getLValuePos(lvalue))
    } else {
      errorList
    }
  }

  private def checkFreeStatement(
      expression: Expression
  )(implicit errorList: List[Error], funcName: String): List[Error] =
    isExpressionArrayOrPairType(expression)

  private def checkExitStatement(
      expression: Expression
  )(implicit errorList: List[Error], funcName: String) =
    checkExpression(expression, SAIntType)

  private def checkPrintStatement(
      expression: Expression
  )(implicit errorList: List[Error], funcName: String) = isValidExpression(
    expression
  )

  private def checkPrintLnStatement(
      expression: Expression
  )(implicit errorList: List[Error], funcName: String) = isValidExpression(
    expression
  )

  private def checkIfStatement(
      condition: Expression,
      thenStatements: List[Statement],
      elseStatements: List[Statement]
  )(implicit
    returnVal: SAType,
    errorList: List[Error],
    funcName: String,
    funcMap: Map[String, String]): List[Error] = {
    val conditionError = checkExpression(condition, SABoolType)
    if (!(conditionError eq errorList)) return conditionError
    symbolTable.enterScope()
    val thenErrors =
      collectErrors(thenStatements, checkStatement)(conditionError)
    symbolTable.exitScope()
    symbolTable.enterScope()
    val elseErrors = collectErrors(elseStatements, checkStatement)(thenErrors)
    symbolTable.exitScope()
    elseErrors
  }

  private def isValidExpression(
      expression: Expression
  )(implicit errorList: List[Error], funcName: String): List[Error] =
    checkExpression(expression, SAAnyType)

  private def checkLValue(lvalue: LValue, typeName: SAType)(
      implicit
      errorList: List[Error],
      funcName: String): List[Error] = lvalue match {
    case id @ Identifier(_) =>
      symbolTable.lookupVarType(id) match {
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
    case _ => throw new Exception("Unexpected LValue")
  }

  private def checkWhileStatement(
      condition: Expression,
      doStatements: List[Statement]
  )(implicit
    returnVal: SAType,
    errorList: List[Error],
    funcName: String,
    funcMap: Map[String, String]): List[Error] = {
    val conditionError = checkExpression(condition, SABoolType)
    if (!(conditionError eq errorList)) return conditionError
    symbolTable.enterScope()
    val statementErrors =
      collectErrors(doStatements, checkStatement)(conditionError)
    symbolTable.exitScope()
    statementErrors
  }

  private def checkBeginStatement(statements: List[Statement])(
      implicit
      returnVal: SAType,
      errorList: List[Error],
      funcName: String,
      funcMap: Map[String, String]): List[Error] = {
    symbolTable.enterScope()
    val statementErrors = collectErrors(statements, checkStatement)
    symbolTable.exitScope()
    statementErrors
  }

  private def checkStatement(statement: Statement)(
      implicit
      returnVal: SAType,
      errorList: List[Error],
      funcName: String,
      funcMap: Map[String, String]): List[Error] = statement match {
    case SkipStatement => errorList
    case DeclarationStatement(typeName, identifier, rvalue) =>
      checkDeclarationStatement(
        convertSyntaxToTypeSys(typeName),
        identifier,
        rvalue
      )
    case AssignmentStatement(lvalue, rvalue) =>
      checkAssignmentStatement(lvalue, rvalue)
    case ReadStatement(lvalue)     => checkReadStatement(lvalue)
    case FreeStatement(expression) => checkFreeStatement(expression)
    case ReturnStatement(expression) =>
      returnVal match {
        case SAAnyType =>
          errorList :+ ReturnFromMainError(getExpressionPos(expression))
        case _ => checkExpression(expression, returnVal)
      }
    case ExitStatement(expression)    => checkExitStatement(expression)
    case PrintStatement(expression)   => checkPrintStatement(expression)
    case PrintLnStatement(expression) => checkPrintLnStatement(expression)
    case IfStatement(condition, thenStatements, elseStatements) =>
      checkIfStatement(condition, thenStatements, elseStatements)
    case WhileStatement(condition, doStatements) =>
      checkWhileStatement(condition, doStatements)
    case BeginStatement(statements) => checkBeginStatement(statements)
    case _                          => throw new Exception("Unexpected Statement")
  }

  // EXPRESSION RELATED FUNCTIONS
  private def bothTypesMatch(
      lhs: Expression,
      rhs: Expression,
      validTypes: List[SAType]
  )(implicit
    errorList: List[Error],
    funcName: String): Either[SAType, List[Error]] =
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
                      errorList :+ TypeError(
                        getExpressionPos(lhs),
                        ll.toString,
                        List(lr.toString)
                      )
                    )
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
      implicit
      errorList: List[Error],
      funcName: String
  ): Either[SAType, List[Error]] =
    symbolTable.lookupVarType(id) match {
      case Left(SAArrayType(arrayType, arity)) => {
        val indicesError = collectErrors(
          indices,
          (x: Expression) => checkExpression(x, SAIntType)
        )
        if (!(indicesError eq errorList))
          Right(indicesError)
        else if (indices.length < arity)
          Left(SAArrayType(arrayType, arity - indices.length))
        else if (indices.length == arity)
          Left(arrayType)
        else
          Right(
            indicesError :+ ArrayArityError(
              id.pos,
              arity - indices.length,
              arity
            )
          )
      }
      case Left(t) =>
        Right(
          errorList :+ TypeError(id.pos, t.toString, List(SAArrayType.toString))
        )
      case rel @ Right(_) => rel
    }
  
  

  private def getUnOpType(op: UnaryOp, expression: Expression)(
      implicit
      errorList: List[Error],
      funcName: String): Either[SAType, List[Error]] = op match {
    case Negation =>
      getTypeIfExpressionsType(List(expression), SAIntType, SAIntType)
    case Chr =>
      getTypeIfExpressionsType(List(expression), SAIntType, SACharType)
    case Len =>
      expression match {
        case id @ Identifier(_) =>
          symbolTable.lookupVarType(id) match {
            case Left(SAArrayType(_, _)) => Left(SAIntType)
            case rel @ Right(_)          => rel
            case _                       => throw new Exception("Unexpected lookup result")
          }
        case ArrayElem(id, indices) =>
          symbolTable.lookupVarType(id) match {
            case Left(SAArrayType(_, arity)) =>
              if (indices.length < arity) {
                Left(SAIntType)
              } else
                Right(
                  errorList :+ ArrayArityError(
                    getExpressionPos(expression),
                    indices.length,
                    arity
                  )
                )
            case rel @ Right(_) => rel
            case _              => throw new Exception("Unexpected lookup result")
          }
        case _ =>
          Right(
            errorList :+ TypeError(
              getExpressionPos(expression),
              expression.toString,
              List(SAArrayType.toString)
            )
          )
      }
    case Not =>
      getTypeIfExpressionsType(List(expression), SABoolType, SABoolType)
    case Ord =>
      getTypeIfExpressionsType(List(expression), SACharType, SAIntType)
  }

  private def getBinOpType(binaryOp: BinaryOpApp)(
      implicit
      errorList: List[Error],
      funcName: String): Either[SAType, List[Error]] =
    binaryOp.op match {
      case Mul | Div | Mod | Plus | Minus =>
        getTypeIfExpressionsType(
          List(binaryOp.lhs, binaryOp.rhs),
          SAIntType,
          SAIntType
        )
      case And | Or =>
        getTypeIfExpressionsType(
          List(binaryOp.lhs, binaryOp.rhs),
          SABoolType,
          SABoolType
        )
      case Gt | Ge | Lt | Le => {
        bothTypesMatch(
          binaryOp.lhs,
          binaryOp.rhs,
          List(SAIntType, SACharType)
        ) match {
          case Left(_) => Left(SABoolType)
          case _ =>
            Right(
              errorList :+ BinaryOpAppTypeError(
                binaryOp.pos,
                List(SAIntType.toString, SACharType.toString)
              )
            )
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
                      _.toString
                    )
                  )
                )
              case Right(errorList) =>
                Right(
                  errorList :+ BinaryOpAppTypeError(
                    binaryOp.pos,
                    List(SAIntType, SACharType, SABoolType, SAStringType).map(
                      _.toString
                    )
                  )
                )
            }
          case Right(errorList) =>
            Right(
              errorList :+ BinaryOpAppTypeError(
                binaryOp.pos,
                List(SAIntType, SACharType, SABoolType, SAStringType).map(
                  _.toString
                )
              )
            )
        }
      }
    }

  private def getExpressionType(expression: Expression)(
      implicit
      errorList: List[Error],
      funcName: String): Either[SAType, List[Error]] =
    expression match {
      case IntLiteral(_)              => Left(SAIntType)
      case BoolLiteral(_)             => Left(SABoolType)
      case CharLiteral(_)             => Left(SACharType)
      case StringLiteral(_)           => Left(SAStringType)
      case PairLiteral                => Left(SAPairRefType)
      case ArrayElem(id, indices)     => getArrayElemType(id, indices)
      case id @ Identifier(_)         => symbolTable.lookupVarType(id)
      case UnaryOpApp(op, expr)       => getUnOpType(op, expr)
      case boa @ BinaryOpApp(_, _, _) => getBinOpType(boa)
      case _                          => throw new Exception("we shouldnt get this!")
    }

  private def checkExpression(expression: Expression, t: SAType)(
      implicit
      errorList: List[Error],
      funcName: String): List[Error] =
    getExpressionType(expression) match {
      case Left(otherType) =>
        equalsTypeNoError(getExpressionPos(expression), t, otherType)
      case Right(el) => el
    }

  // FUNCTION RELATED
  private def mapDefs(
      function: Func
  )(implicit errorList: List[Error]): List[Error] = {
    val retType = convertSyntaxToTypeSys(function.identBinding.typeName)
    val params = function.params.map(_.typeName).map(convertSyntaxToTypeSys)
    functionTable.insertFunction(function, TypeSignature(retType, params))
  }

  private def checkFunctionCall(
      id: Identifier,
      args: List[Expression],
      typeName: SAType
  )(implicit errorList: List[Error], funcName: String, funcMap: Map[String, String]): List[Error] = {
    val allowedTypeSignatures = functionTable.getAllowedTypeSignatures(id) match {
      case Left(x) => x
      case Right(x) => return x ++ errorList
    }
    val currentParameterTypes = ListBuffer[SAType]()

    val paramErrorList = ListBuffer[Error]()
    args.foreach((arg) => getExpressionType(arg) match {
      case Left(x) => currentParameterTypes.append(x)
      case Right(el) => paramErrorList ++= el
    })
    if (paramErrorList.nonEmpty) {
      return paramErrorList.toList ++ errorList
    }

    val currentTypeSignature = TypeSignature(typeName, currentParameterTypes.toList)
    if (allowedTypeSignatures.contains(currentTypeSignature)) {
      errorList
    } else {
      errorList :+ FunctionCallTypeError(
        id.pos,
        id.name,
        allowedTypeSignatures.map(_.toString()).toList,
        currentTypeSignature.toString()
      )
    }
  }

  private def checkFunction(
      func: Func
  )(implicit errorList: List[Error], funcMap: Map[String, String]): List[Error] = {
    implicit val funcName = renameFunctionName(
      func.identBinding.identifier,
      TypeSignature(
        convertSyntaxToTypeSys(func.identBinding.typeName),
        func.params.map(_.typeName).map(convertSyntaxToTypeSys)
      ),
      functionTable
    ).name
    symbolTable.insertFunction(funcName)
    def collectErrorsFunctionStatements(a: List[Statement], returnVal: SAType)(
        implicit errorList: List[Error]
    ): List[Error] = {
      a match {
        case a :: b =>
          collectErrorsFunctionStatements(b, returnVal)(
            checkStatement(a)(returnVal, errorList, funcName, funcMap)
          )
        case Nil => errorList
      }
    }
    // add all params to symbol table, now in scope
    val params = func.params
    val paramErrors = collectErrors(
      params,
      (p: Parameter) =>
        symbolTable.insertVar(p.identifier, convertSyntaxToTypeSys(p.typeName))
    )
    if (!(paramErrors eq errorList)) {
      return paramErrors
    }
    symbolTable.enterScope()
    val functionReturnType: SAType = convertSyntaxToTypeSys(func.identBinding.typeName)
    val funcErrors =
      collectErrorsFunctionStatements(func.body, functionReturnType)
    symbolTable.exitScope()
    funcErrors
  }

  private def checkFunctions(
      program: Program
  )(implicit errorList: List[Error], funcMap: Map[String, String]): List[Error] = {
    collectErrors(program.functions, checkFunction)(
      collectErrors(program.functions, mapDefs)
    )
  }

  def renameFunctionName(funcId: Identifier, typeSignature: TypeSignature, functionTable: FunctionTable): Identifier = {
    def renameFunctionRule(name: String, funcNo: Int): String = {
      name + "_" + funcNo
    }
    funcId match {
      case Identifier(name) => Identifier(renameFunctionRule(name, functionTable.getFunctionNo(name, typeSignature)))((0, 0))
    }
  }

  private def getExpressionTypes(args: List[Expression])(implicit errorList: List[Error], funcName: String): List[SAType] = {
    args.map((x) => getExpressionType(x) match {
      case Left(t) => t
      case Right(el) => throw new Exception("Function Overloading Error")
    })
  }

  def renameOverloadedFunctionCalls(statements: List[Statement], functionTable: FunctionTable)(implicit errorList: List[Error], funcName: String): List[Statement] = 
    statements.map(s => s match {
      case AssignmentStatement(lhs, FunctionCall(id, args)) =>
        getLValueType(lhs) match {
          case Left(t) => AssignmentStatement(
            lhs,
            FunctionCall(renameFunctionName(id, TypeSignature(t, getExpressionTypes(args)),
            functionTable),
            args)((0, 0))
          )((0, 0))
          case Right(el) => s // change to throw error
        }
      case DeclarationStatement(typeName, id, FunctionCall(funcId, args)) => {
        symbolTable.insertVar(id, convertSyntaxToTypeSys(typeName));
        DeclarationStatement(
          typeName,
          id,
          FunctionCall(renameFunctionName(funcId, TypeSignature(convertSyntaxToTypeSys(typeName), getExpressionTypes(args)),
          functionTable),
          args)((0, 0))
        )((0, 0))
      }
      case DeclarationStatement(typeName, id, rvalue) => {
        symbolTable.insertVar(id, convertSyntaxToTypeSys(typeName)); s
      }
      case IfStatement(cond, thenBlock, elseBlock) => {
        symbolTable.enterScope()
        val updatedThenBlock = renameOverloadedFunctionCalls(thenBlock, functionTable)
        symbolTable.exitScope()
        symbolTable.enterScope()
        val updatedElseBlock = renameOverloadedFunctionCalls(elseBlock, functionTable)
        symbolTable.exitScope()
        IfStatement(
          cond,
          updatedThenBlock,
          updatedElseBlock
        )((0, 0))
      }
      case BeginStatement(statements) => {
        symbolTable.enterScope()
        val updatedStatements = renameOverloadedFunctionCalls(statements, functionTable)
        symbolTable.exitScope()
        BeginStatement(updatedStatements)((0, 0))
      }
      case WhileStatement(cond, body) => {
        symbolTable.enterScope()
        val updatedBody = renameOverloadedFunctionCalls(body, functionTable)
        symbolTable.exitScope()
        WhileStatement(cond, updatedBody)((0, 0))
      }
      case _ => s
    })

  def renameOverloadedFunctionDefinition(func: Func, functionTable: FunctionTable)(implicit errorList: List[Error]): Func = 
    func match {
      case Func(IdentBinding(retType, funcId), params, body) => {
        val newFunctionName = renameFunctionName(funcId,
        TypeSignature(convertSyntaxToTypeSys(retType), params.map(p => p match {
          case Parameter(paramType, _) => convertSyntaxToTypeSys(paramType)
        })), functionTable)
        implicit val funcName = newFunctionName.name
        symbolTable.enterScope()
        val updatedFunctionBody = renameOverloadedFunctionCalls(body, functionTable)
        symbolTable.exitScope()
        Func(IdentBinding(retType, newFunctionName)((0, 0)), params, updatedFunctionBody)((0, 0))
      }
    }

  def renameOverloadedFunctions(program: Program, functionTable: FunctionTable)(implicit errorList: List[Error], funcName: String): Program = 
    Program(program.imports, program.functions.map(renameOverloadedFunctionDefinition(_, functionTable)), renameOverloadedFunctionCalls(program.statements, functionTable))((0, 0))

  def insertLibFuns(importRef: Import)(implicit libToFuncMap: Map[String, List[(String, (Type, List[Type]))]], errorList: List[Error]) = {
    libToFuncMap.get(importRef.importName) match {
      case Some(funs) =>
        funs.foreach(f => {
          functionTable.insertFunction(Func(IdentBinding(f._2._1, Identifier(f._1)(0,0))(0,0), List(Parameter(StringType, Identifier("x")(0,0))(0,0)), List())(0,0), TypeSignature(convertSyntaxToTypeSys(f._2._1), f._2._2.map(convertSyntaxToTypeSys)))
          symbolTable.insertFunction(renameFunctionName(Identifier(f._1)((0, 0)), TypeSignature(convertSyntaxToTypeSys(f._2._1), f._2._2.map(convertSyntaxToTypeSys)), functionTable).name)
        })
      case None => throw new Exception("Library not found")
    }
  }

  // Main function to run semantic analysis, return errors as state
  def checkProgram(
      program: Program
  )(implicit funcToLibMap: Map[String, String], libToFuncMap: Map[String, List[(String, (Type, List[Type]))]]): (List[Error], SymbolTable, FunctionTable, Program) = {
    implicit val errorList: List[Error] = List()
    implicit val funcName: String = "0"

    // add imported type signatures to function table
    program.imports.foreach(insertLibFuns)

    symbolTable.insertFunction(funcName)
    val copySymbolTable = symbolTable.deepcopy()
    symbolTable.enterScope()
    val errors =
      collectErrors(program.statements, (x: Statement) => checkStatement(x))(
        checkFunctions(program)
      )
    symbolTable.exitScope()
    if (errors.nonEmpty) {
      return (errors, symbolTable, functionTable, program)
    }
    copySymbolTable.enterScope()
    val updatedProgram = renameOverloadedFunctions(program, functionTable)
    copySymbolTable.exitScope()
    (errors, symbolTable, functionTable, updatedProgram)
  }
}
