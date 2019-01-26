package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Accumulator, MixByte, Field, Index1, Mix, TwoSignedBytes, Word}

case class INC1(override val value: Word) extends INC {

  val name = "INC1"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index1, mix.i1).incrementProgramCounter

}

object INC1 {

  val operationCode = MixByte(49)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INC1 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INC1 =
    INC1(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}