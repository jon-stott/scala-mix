package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index4, Mix, TwoSignedBytes, Word}

case class INC4(override val value: Word) extends INC {

  val name = "INC4"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index4, mix.i4).incrementProgramCounter

}

object INC4 {

  val operationCode = MixByte(52)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INC4 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INC4 =
    INC4(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}