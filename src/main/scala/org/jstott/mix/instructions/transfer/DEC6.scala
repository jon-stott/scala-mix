package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index6, Mix, TwoSignedBytes, Word}

case class DEC6(override val value: Word) extends DEC {

  val name = "DEC6"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index6, mix.i6).incrementProgramCounter

}

object DEC6 {

  val operationCode = MixByte(55)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DEC6 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DEC6 =
    DEC6(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}