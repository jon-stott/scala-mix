package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Index1, Mix, TwoSignedBytes, Word}

case class DEC1(override val value: Word) extends DEC {

  val name = "DEC1"

  def compute(implicit mix: Mix): Mix = computeTwoSignedBytes(mix, Index1, mix.i1).incrementProgramCounter

}

object DEC1 {

  val operationCode = MixByte(49)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): DEC1 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): DEC1 =
    DEC1(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}