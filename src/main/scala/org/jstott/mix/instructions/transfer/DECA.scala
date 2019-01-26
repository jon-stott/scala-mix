package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Accumulator, MixByte, Field, Mix, TwoSignedBytes, Word}

case class DECA(override val value: Word) extends DEC {

  val name = "DECA"

  def compute(implicit mix: Mix): Mix = computeWord(mix, Accumulator, mix.a).incrementProgramCounter

}

object DECA {

  val operationCode = MixByte(48)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DECA = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DECA =
    DECA(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}