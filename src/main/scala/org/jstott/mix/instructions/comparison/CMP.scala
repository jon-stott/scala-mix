package org.jstott.mix.instructions.comparison

import org.jstott.mix._
import org.jstott.mix.instructions.Instruction

trait CMP extends Instruction {

  def computeNewComparisonIndicator(mix: Mix, registerValue: Int): ComparisonIndicator = {
    val toCompare = mix.contents(A, I).intValue
    if (toCompare == registerValue) {
      Equal
    } else if (registerValue < toCompare) {
      Less
    } else {
      Greater
    }
  }

}
