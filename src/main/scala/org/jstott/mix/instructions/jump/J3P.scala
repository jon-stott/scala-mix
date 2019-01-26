package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J3P(override val value: Word) extends J {

  val name = "J3P"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.index3)

}

object J3P {

  val operationCode = MixByte(43)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): J3P = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J3P =
    J3P(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}