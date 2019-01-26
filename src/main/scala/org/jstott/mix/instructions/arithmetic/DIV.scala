package org.jstott.mix.instructions.arithmetic

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{MixByte, DWord, Field, Mix, On, TwoSignedBytes, Word}

case class DIV(override val value: Word) extends Instruction {

  val name = "DIV"

  def compute(implicit mix: Mix): Mix = {
    val v = applyFieldToWord(mix.contents(A, I))
    val ax = DWord(mix.accumulator, mix.extension)
    if (v.intValue == 0) {
      mix.withOverflow(On).incrementProgramCounter
    } else {
      val quotient = ax.longValue / v.longValue
      val remainder = ax.longValue % v.longValue
      mix.copy(accumulator = Word(quotient.toInt), extension = Word(remainder.toInt).withSign(mix.accumulator.sign))
        .incrementProgramCounter
    }
  }

}

object DIV {

  val operationCode = MixByte(4)

  def apply(address: TwoSignedBytes): DIV = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): DIV = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): DIV = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): DIV =
    DIV(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}