package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index2, Mix, TwoSignedBytes, Word}

case class INC2(override val value: Word) extends INC {

  val name = "INC2"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index2, mix.i2).incrementProgramCounter

}

object INC2 {

  val operationCode = MixByte(50)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INC2 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INC2 =
    INC2(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}