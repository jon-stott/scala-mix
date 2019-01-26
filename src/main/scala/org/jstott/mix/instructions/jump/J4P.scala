package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J4P(override val value: Word) extends J {

  val name = "J4P"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.index4)

}

object J4P {

  val operationCode = MixByte(44)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): J4P = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J4P =
    J4P(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}