package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Mix, Word}
import org.jstott.mix.instructions.Instruction

trait ENT extends Instruction {

  def computeValue(mix: Mix): Word = {
    val mVal = M.intValue
    val iVal = mix.i(I).intValue
    Word(mVal + iVal)
  }

}