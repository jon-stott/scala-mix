package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index5, Mix, TwoSignedBytes, Word}

case class DEC5(override val value: Word) extends DEC {

  val name = "DEC5"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index5, mix.i5).incrementProgramCounter

}

object DEC5 {

  val operationCode = MixByte(53)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DEC5 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DEC5 =
    DEC5(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}