package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index5, Mix, TwoSignedBytes, Word}

case class INC5(override val value: Word) extends INC {

  val name = "INC5"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index5, mix.i5).incrementProgramCounter

}

object INC5 {

  val operationCode = MixByte(53)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INC5 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INC5 =
    INC5(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}