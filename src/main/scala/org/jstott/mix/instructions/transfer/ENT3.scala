package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENT3(override val value: Word) extends ENT {

  val name = "ENT3"

  def compute(implicit mix: Mix): Mix = mix.copy(index3 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENT3 {

  val operationCode = MixByte(51)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENT3 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENT3 =
    ENT3(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}