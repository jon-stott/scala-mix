package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Accumulator, MixByte, Extension, Field, Mix, TwoSignedBytes, Word}

case class DECX(override val value: Word) extends DEC {

  val name = "DECX"

  def compute(implicit mix: Mix): Mix = computeWord(mix, Extension, mix.x).incrementProgramCounter

}

object DECX {

  val operationCode = MixByte(55)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DECX = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DECX =
    DECX(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}