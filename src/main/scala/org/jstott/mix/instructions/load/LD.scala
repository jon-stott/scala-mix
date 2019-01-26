package org.jstott.mix.instructions.load

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{Mix, Plus, Word}

trait LD extends Instruction {

  protected def computeLoad(mix: Mix, invertSign: Boolean = false): Word = {
    applyFieldToWord(mix.contents(A, I), invertSign)
  }

}
