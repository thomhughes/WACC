package wacc

object ControlFlowOptimization {
    import wacc.AST._
    
    def optimizeControlFlow(program: Program): Program =
        Program(
            program.imports,
            program.functions.map(optimizeControlFlowFunction(_)),
            optimizeControlFlowStatements(program.statements)
        )((0, 0))

    def optimizeControlFlowStatements(statements: List[Statement]): List[Statement] = 
        statements match {
            case IfStatement(BoolLiteral(true), thenBody, elseBody) :: ss => BeginStatement(optimizeControlFlowStatements(thenBody))((0, 0)) :: optimizeControlFlowStatements(ss)
            case IfStatement(BoolLiteral(false), thenBody, elseBody) :: ss => BeginStatement(optimizeControlFlowStatements(elseBody))((0, 0)) :: optimizeControlFlowStatements(ss)
            case IfStatement(cond, thenBody, elseBody) :: ss => IfStatement(cond, optimizeControlFlowStatements(thenBody), optimizeControlFlowStatements(elseBody))((0, 0)) :: optimizeControlFlowStatements(ss)
            case WhileStatement(BoolLiteral(true), body) :: ss => WhileStatement(BoolLiteral(true)((0, 0)), optimizeControlFlowStatements(body))((0, 0)) :: Nil
            case WhileStatement(BoolLiteral(false), body) :: ss => optimizeControlFlowStatements(ss)
            case WhileStatement(cond, body) :: ss => WhileStatement(cond, optimizeControlFlowStatements(body))((0, 0)) :: optimizeControlFlowStatements(ss)
            case BeginStatement(statements) :: ss => BeginStatement(optimizeControlFlowStatements(statements))((0, 0)) :: optimizeControlFlowStatements(ss)
            case ReturnStatement(expr) :: ss => ReturnStatement(expr)((0, 0)) :: Nil
            case ExitStatement(expr) :: ss => ExitStatement(expr)((0, 0)) :: Nil
            case s :: ss => s :: optimizeControlFlowStatements(ss)
            case Nil => Nil
        }

    def optimizeControlFlowFunction(function: Func): Func =
        function match {
            case Func(identBinding, params, body) => Func(identBinding, params, optimizeControlFlowStatements(body))((0, 0))
        }
}
