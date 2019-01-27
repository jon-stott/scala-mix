package org.jstott.mix.instructions.transfer

import org.jstott.mix.instructions.Instruction
import org.jstott.mix._

trait INC extends Instruction {

  def computeWord(mix: Mix, register: Register, registerValue: Word): Mix = {
    val newValue = registerValue.intValue + addressWithIndex(mix)
    val mixWithOverflow = if (newValue > Word.maxValue) mix.withOverflow(On) else mix
    mixWithOverflow.withRegister(register, Word(newValue))
  }

  def computeTwoSignedBytes(mix: Mix, register: Register, registerValue: TwoSignedBytes): Mix = {
    val newValue = registerValue.intValue + addressWithIndex(mix)
    if (newValue > TwoSignedBytes.maxValue) {
      throw new UnexpectedOverflowException(s"new value [$newValue] caused overflow in register [$register]")
    }
    mix.withRegister(register, TwoSignedBytes(newValue))
  }

}