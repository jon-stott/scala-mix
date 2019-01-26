package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENTA(override val value: Word) extends ENT {

  val name = "ENTA"

  def compute(implicit mix: Mix): Mix = mix.copy(accumulator = computeValue(mix)).incrementProgramCounter

}

object ENTA {

  val operationCode = MixByte(48)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENTA = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENTA =
    ENTA(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}