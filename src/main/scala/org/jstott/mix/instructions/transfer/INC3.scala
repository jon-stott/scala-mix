package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index3, Mix, TwoSignedBytes, Word}

case class INC3(override val value: Word) extends INC {

  val name = "INC3"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index3, mix.i3).incrementProgramCounter

}

object INC3 {

  val operationCode = MixByte(51)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INC3 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INC3 =
    INC3(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}