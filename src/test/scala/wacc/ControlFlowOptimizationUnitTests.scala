package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST._
import org.scalatest.matchers.should.Matchers._
import wacc.ControlFlowOptimization.optimizeControlFlow

class ControlFlowOptimizationUnitTests extends AnyFlatSpec {
    "Control flow optimizer" should "optimize if statement with true literal" in {
        val program = Program(List[Import](), List[Func](),
        List(
            IfStatement(BoolLiteral(true)((0, 0)),
            List(ExitStatement(IntLiteral(3)((0, 0)))((0, 0))),
            List(ExitStatement(IntLiteral(5)((0, 0)))((0, 0))))((0, 0))
        ))((0, 0))
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(), List(BeginStatement(List(
                ExitStatement(IntLiteral(3))
            )))) => 
        }
    }

    "Control flow optimizer" should "optimize if statement with false literal" in {
        val program = Program(List[Import](), List[Func](),
        List(
            IfStatement(BoolLiteral(false)((0, 0)),
            List(ExitStatement(IntLiteral(3)((0, 0)))((0, 0))),
            List(ExitStatement(IntLiteral(5)((0, 0)))((0, 0))))((0, 0))))((0, 0))
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(), List(BeginStatement(List(
                ExitStatement(IntLiteral(5))
            )))) => 
        }
    }

    "Control flow optimizer" should "optimize while statement with true literal" in {
        val program = Program(List[Import](), List[Func](),
        List(
            WhileStatement(BoolLiteral(true)((0, 0)), List(DeclarationStatement(IntType, Identifier("x")(0,0), IntLiteral(4)(0,0))(0,0)))(0,0),
            DeclarationStatement(IntType, Identifier("y")(0,0), IntLiteral(4)(0,0))((0, 0))
        ))(0, 0)
        optimizeControlFlow(program) should matchPattern {
            case Program(List(), List(), List(
                WhileStatement(BoolLiteral(true), List(DeclarationStatement(IntType, Identifier("x"), IntLiteral(4)))))) => 
        }
    }


    "Control flow optimizer" should "optimize while statement with false literal" in {
        val program = Program(List[Import](), List[Func](),
        List(
            WhileStatement(BoolLiteral(false)((0, 0)),
            List(ExitStatement(IntLiteral(3)((0, 0)))((0, 0))))((0, 0))))((0, 0))
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(), List()) => 
        }
    }
    
    "Control flow optimizer" should "optimize statements after return statements" in {
       val program = Program(List[Import](), List(
        Func(IdentBinding(IntType, Identifier("function")(0, 0))((0, 0)), List(), List(
            ReturnStatement(IntLiteral(3)((0, 0)))((0, 0)),
            ExitStatement(IntLiteral(5)((0, 0)))((0, 0))
        ))((0, 0))
       ), List())(0, 0)
       optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(Func(IdentBinding(IntType, Identifier("function")), List(), List(
                ReturnStatement(IntLiteral(3))))), List()) => 
        }
    }

    "Control flow optmimizer" should "optimize statements after exit statements" in {
        val program = Program(List[Import](), List[Func](), List(
            ExitStatement(IntLiteral(0)(0, 0))(0, 0),
            ExitStatement(IntLiteral(1)(0, 0))(0, 0)
        ))(0, 0)
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(), List(
                ExitStatement(IntLiteral(0))
            )) => 
        }
    }
    
    "Control flow optimizer" should "optimize statements inside scope of if statements" in {
        val program = Program(List[Import](), List[Func](),
            List(
                IfStatement(Identifier("x")(0, 0),
                    List(
                        IfStatement(
                            BoolLiteral(false)((0, 0)),
                            List(ExitStatement(IntLiteral(1)((0, 0)))((0, 0))),
                            List(ExitStatement(IntLiteral(2)((0, 0)))((0, 0)))
                        )((0, 0))
                    ),
                    List(
                        IfStatement(
                            BoolLiteral(true)((0, 0)),
                            List(ExitStatement(IntLiteral(3)((0, 0)))(0, 0)),
                            List(ExitStatement(IntLiteral(4)((0, 0)))((0, 0)))
                        )((0, 0))
                    )
                )(0, 0)
            )
        )((0, 0))
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(),
              List(
                IfStatement(Identifier("x"),
                    List(
                        BeginStatement(List(ExitStatement(IntLiteral(2))))
                    ),
                    List(
                        BeginStatement(List(ExitStatement(IntLiteral(3))))
                    )
                ))) => 
        }
    }

    "Control flow optimizer" should "optimize statements inside scope of while statements" in {
        val program = Program(List[Import](), List[Func](),
        List(
            WhileStatement(
                BoolLiteral(true)((0, 0)),
                List(
                    IfStatement(
                        BoolLiteral(true)((0, 0)),
                        List(ExitStatement(IntLiteral(3)((0, 0)))((0, 0))),
                        List(ExitStatement(IntLiteral(5)((0, 0)))((0, 0)))
                    )((0, 0))
                )
            )(0, 0)
        ))(0, 0)
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(), List(
                WhileStatement(BoolLiteral(true), List(BeginStatement(List(ExitStatement(IntLiteral(3)))))))
            ) => 
        }
    }

    "Control flow optimizer" should "optimize statements inside scope of begin statements" in {
        val program = Program(List[Import](), List[Func](),
        List(
            BeginStatement(
                List(
                    IfStatement(BoolLiteral(true)((0, 0)),
                    List(ExitStatement(IntLiteral(3)((0, 0)))((0, 0))),
                    List(ExitStatement(IntLiteral(5)((0, 0)))((0, 0))))((0, 0))
                )
            )(0, 0)
        ))(0, 0)
        optimizeControlFlow(program) should matchPattern { case 
            Program(List(), List(), List(
                BeginStatement(List(BeginStatement(List(
                    ExitStatement(IntLiteral(3))
                ))))
            )) => 
        }
    }
}
