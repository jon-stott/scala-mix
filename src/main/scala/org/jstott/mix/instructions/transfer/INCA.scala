package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Accumulator, MixByte, Field, Mix, TwoSignedBytes, Word}

case class INCA(override val value: Word) extends INC {

  val name = "INCA"

  def compute(implicit mix: Mix): Mix = computeWord(mix, Accumulator, mix.a).incrementProgramCounter

}

object INCA {

  val operationCode = MixByte(48)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INCA = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INCA =
    INCA(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}