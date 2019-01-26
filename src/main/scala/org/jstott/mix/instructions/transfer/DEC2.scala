package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index2, Mix, TwoSignedBytes, Word}

case class DEC2(override val value: Word) extends DEC {

  val name = "DEC2"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index2, mix.i2).incrementProgramCounter

}

object DEC2 {

  val operationCode = MixByte(50)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DEC2 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DEC2 =
    DEC2(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}