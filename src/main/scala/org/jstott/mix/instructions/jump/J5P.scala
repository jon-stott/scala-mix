package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J5P(override val value: Word) extends J {

  val name = "J5P"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.index5)

}

object J5P {

  val operationCode = MixByte(45)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): J5P = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J5P =
    J5P(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}