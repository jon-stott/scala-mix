package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENT2(override val value: Word) extends ENT {

  val name = "ENT2"

  def compute(implicit mix: Mix): Mix = mix.copy(index2 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENT2 {

  val operationCode = MixByte(50)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENT2 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENT2 =
    ENT2(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}