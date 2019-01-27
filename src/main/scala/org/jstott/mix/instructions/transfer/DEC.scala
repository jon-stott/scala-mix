package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.jstott.mix.instructions.Instruction

trait DEC extends Instruction {

  def computeWord(mix: Mix, register: Register, registerValue: Word): Mix = {
    val newValue = registerValue.intValue - addressWithIndex(mix)
    val mixWithOverflow = if (newValue < Word.minValue) mix.withOverflow(On) else mix
    mixWithOverflow.withRegister(register, Word(newValue))
  }

  def computeTwoSignedBytes(mix: Mix, register: Register, registerValue: TwoSignedBytes): Mix = {
    val newValue = registerValue.intValue - addressWithIndex(mix)
    if (newValue < TwoSignedBytes.minValue) {
      throw new UnexpectedOverflowException(s"new value [$newValue] caused overflow in register [$register]")
    }
    mix.withRegister(register, TwoSignedBytes(newValue))
  }

}