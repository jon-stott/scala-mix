package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENT5(override val value: Word) extends ENT {

  val name = "ENT5"

  def compute(implicit mix: Mix): Mix = mix.copy(index5 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENT5 {

  val operationCode = MixByte(53)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENT5 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENT5 =
    ENT5(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}