package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENT4(override val value: Word) extends ENT {

  val name = "ENT4"

  def compute(implicit mix: Mix): Mix = mix.copy(index4 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENT4 {

  val operationCode = MixByte(52)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENT4 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENT4 =
    ENT4(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}