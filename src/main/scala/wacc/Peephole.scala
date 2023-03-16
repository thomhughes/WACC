package wacc

object Peephole {
  import scala.collection.mutable.ListBuffer

  def peepholeOptimisationInstr(instr: IRType, nextInstr: Option[IRType])(
      implicit optimisedInstructions: ListBuffer[IRType]): Boolean =
    (instr, nextInstr) match {
      case (Instr(MOV, Some(fstOp), Some(sndOp), None, _), _)
          if fstOp == sndOp =>
        false
      case (i, None) => {
        optimisedInstructions += i
        false
      }
      case (fi, Some(si)) =>
        (fi, si) match {
          case (Instr(MOV, Some(fstAOp), Some(fstBOp), None, fstCond),
                Instr(MOV, Some(sndBOp), Some(sndAOp), None, sndCond))
              if fstAOp == sndAOp && sndBOp == fstBOp && fstCond == sndCond =>
            optimisedInstructions += Instr(MOV,
                                           fstAOp.asInstanceOf[Register],
                                           fstBOp,
                                           fstCond)
            true
          case (Instr(PUSH, Some(pushRegList), None, None, _),
                Instr(POP, Some(popRegList), None, None, _))
              if pushRegList == popRegList =>
            true
          case (Instr(STR, Some(fstRegOp), Some(fstMemOp), None, _),
                Instr(LDR, Some(sndRegOp), Some(sndMemOp), None, _))
              if fstRegOp == sndRegOp && fstMemOp == sndMemOp =>
            optimisedInstructions += Instr(STR,
                                           fstRegOp.asInstanceOf[Register],
                                           fstMemOp)
            true
          case (Instr(LDR, Some(fstRegOp), Some(fstMemOp), None, _),
                Instr(LDR, Some(sndRegOp), Some(sndMemOp), None, _))
              if fstRegOp == sndRegOp && fstMemOp == sndMemOp =>
            optimisedInstructions += Instr(LDR,
                                           fstRegOp.asInstanceOf[Register],
                                           fstMemOp)
            true
          case default =>
            optimisedInstructions += fi
            false
        }
    }

  def peepholeOptimisation(
      instructions: ListBuffer[IRType]): ListBuffer[IRType] = {
    implicit val optimisedInstructions = ListBuffer[IRType]()
    var counter = 0
    while (counter < instructions.length) {
      if (peepholeOptimisationInstr(instructions(counter),
                                    if (counter + 1 < instructions.length)
                                      Some(instructions(counter + 1))
                                    else None)) {
        counter += 1
      }
      counter += 1
    }
    optimisedInstructions
  }
}
