package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENT6(override val value: Word) extends ENT {

  val name = "ENT6"

  def compute(implicit mix: Mix): Mix = mix.copy(index6 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENT6 {

  val operationCode = MixByte(54)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENT6 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENT6 =
    ENT6(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}