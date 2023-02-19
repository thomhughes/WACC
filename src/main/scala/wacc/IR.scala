// package wacc

// import scala.collection.immutable.Vector

// object IR {
//   import AST._


// //   def buildMain

//   // TODO: Finish implementing this
//   def convertLValueToOperand(lvalue: LValue): Option[Operand] = {
//     lvalue match {
//       case Identifier(name) => Some(Var(name))
//       case default => None
//     }
//   }

//   // TODO: Finish implementing this
//   def convertRValueToOperand(rvalue: RValue): Option[Operand] = {
//     rvalue match {
//       case IntLiteral(value) => Some(Imm(value))
//       case BinaryOpApp(op, lhs, rhs) => 
//       case default => None
//     }
//   }

//   def buildExit(exitStatement: ExitStatement)(implicit ir: Vector[IRType]): Vector[IRType] = exitStatement match {
//     case ExitStatement(statement) => statement match {
//       case IntLiteral(exitCode) => ir :+ Instr(MOV, Some(Imm(exitCode)))
//       case Identifier(varName) => {
//         // TODO: perform lookup and append
//       }
//       case _ => throw new Exception("Exit code is non-integer")
//     }
//     ir :+ Instr(BL, Some(LabelRef("exit")))
//   }

//   def buildStatement(statement: Statement): Vector[IRType] = statement match {
//     case AssignmentStatement(lvalue, rvalue) => {
//       val acc = Vector[IRType]()
//       acc :+ Instr(STR, convertLValueToOperand(lvalue), convertRValueToOperand(rvalue) match {
        
//       }, None)
//     }
//     case DeclarationStatement(typeName, id, rvalue) => {
//       val acc = Vector[IRType]()
//       acc :+ Instr(STR, convertLValueToOperand(lvalue), convertRValueToOperand(rvalue), None)
//     }
//   }


//   def collector[A, B](list: List[A], func: A => Vector[B], acc: Vector[B]): Vector[B] = list match {
//     case head :: next => collector(next, func, acc ++ func(head))
//     case Nil          => acc
//   }

//   def buildFunc(func: Func): Vector[IRType] = {
//     val acc = Vector[IRType]()
//     acc :+ Label(func.identBinding.identifier.name)
//     collector(func.body, buildStatement, acc)
//   } 

//   def buildFuncs(functions: List[Func])(implicit ir: Vector[IRType]): Vector[IRType] = {
//     ir
//   }

//   def buildIR(ast: Program): Vector[IRType] = {
//     implicit val ir = Vector[IRType]()
//     buildFuncs(ast.functions)
//     // buildMain(ast.statements)
//   }
// }
