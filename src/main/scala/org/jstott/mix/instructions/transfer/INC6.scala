package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index6, Mix, TwoSignedBytes, Word}

case class INC6(override val value: Word) extends INC {

  val name = "INC6"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index6, mix.i6).incrementProgramCounter

}

object INC6 {

  val operationCode = MixByte(54)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INC6 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INC6 =
    INC6(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}