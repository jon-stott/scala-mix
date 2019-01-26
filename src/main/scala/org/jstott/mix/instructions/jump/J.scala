package org.jstott.mix.instructions.jump

import org.jstott.mix._
import org.jstott.mix.instructions.Instruction

trait J extends Instruction {

  protected def j_n(mix: Mix, register: SignedBytes): Mix = doJump(mix, register.intValue < 0)

  protected def j_z(mix: Mix, register: SignedBytes): Mix = doJump(mix, register.intValue == 0)

  protected def j_p(mix: Mix, register: SignedBytes): Mix = doJump(mix, register.intValue > 0)

  protected def j_nn(mix: Mix, register: SignedBytes): Mix = doJump(mix, register.intValue >= 0)

  protected def j_nz(mix: Mix, register: SignedBytes): Mix = doJump(mix, register.intValue != 0)

  protected def j_np(mix: Mix, register: SignedBytes): Mix = doJump(mix, register.intValue <= 0)

  protected def doJump(mix: Mix, condition: Boolean): Mix =
    if (condition) {
      mix.copy(
        programCounter = A.intValue,
        jump = TwoUnsignedBytes.fromInt(mix.programCounter + 1)
      )
    } else {
      mix.incrementProgramCounter
    }

}
