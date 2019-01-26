package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index3, Mix, TwoSignedBytes, Word}

case class DEC3(override val value: Word) extends DEC {

  val name = "DEC3"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index3, mix.i3).incrementProgramCounter

}

object DEC3 {

  val operationCode = MixByte(51)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DEC3 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DEC3 =
    DEC3(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}