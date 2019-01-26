package org.jstott.mix.instructions.conversion

import org.jstott.mix.instructions.Instruction
import org.jstott.mix._

case class CHAR(override val value: Word) extends Instruction {

  val name = "CHAR"

  def compute(implicit mix: Mix): Mix = {
    val value = Math.abs(mix.accumulator.intValue)
    val valueStr = f"$value%010d"
    val bytes = valueStr.map(MixByte.fromCharacter)
    mix.withRegister(Accumulator, Word(mix.accumulator.sign, bytes.take(5)))
      .withRegister(Extension, Word(mix.extension.sign, bytes.takeRight(5)))
      .incrementProgramCounter
  }

}

object CHAR {

  val operationCode = MixByte(5)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): CHAR = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): CHAR =
    CHAR(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}