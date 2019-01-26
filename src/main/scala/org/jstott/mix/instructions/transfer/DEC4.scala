package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index4, Mix, TwoSignedBytes, Word}

case class DEC4(override val value: Word) extends DEC {

  val name = "DEC4"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index4, mix.i4).incrementProgramCounter

}

object DEC4 {

  val operationCode = MixByte(52)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DEC4 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DEC4 =
    DEC4(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}