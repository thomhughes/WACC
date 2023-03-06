package wacc

object Peephole {
    import scala.collection.immutable.List

    def peepholeOptimisation(instructions: List[IRType]): List[IRType] =
        instructions match {
            case Nil => Nil
            case (Instr(MOV, Some(fstOp), Some(sndOp), None, _) :: is) if fstOp == sndOp => peepholeOptimisation(is)
            case fi :: Nil => fi :: Nil
            case fi :: (nis @ (si :: is)) => {
                (fi, si) match {
                    case (Instr(MOV, Some(fstAOp), Some(fstBOp), None, fstCond), Instr(MOV, Some(sndBOp), Some(sndAOp), None, sndCond)) if fstAOp == sndAOp && sndBOp == fstBOp && fstCond == sndCond => 
                        Instr(MOV, fstAOp.asInstanceOf[Register], fstBOp, fstCond) :: peepholeOptimisation(is)
                    case (Instr(PUSH, Some(pushRegList), None, None, _), Instr(POP, Some(popRegList), None, None, _)) if pushRegList == popRegList =>
                        peepholeOptimisation(is)
                    case (Instr(STR, Some(fstRegOp), Some(fstMemOp), None, _), Instr(LDR, Some(sndRegOp), Some(sndMemOp), None, _)) if fstRegOp == sndRegOp && fstMemOp == sndMemOp =>
                        Instr(STR, fstRegOp.asInstanceOf[Register], fstMemOp) :: peepholeOptimisation(is)
                    case (Instr(LDR, Some(fstRegOp), Some(fstMemOp), None, _), Instr(LDR, Some(sndRegOp), Some(sndMemOp), None, _)) if fstRegOp == sndRegOp && fstMemOp == sndMemOp =>
                        Instr(LDR, fstRegOp.asInstanceOf[Register], fstMemOp) :: peepholeOptimisation(is)
                    case default =>
                        fi :: peepholeOptimisation(nis)
                }
            }
        }
}
