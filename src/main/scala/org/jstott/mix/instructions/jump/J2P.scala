package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J2P(override val value: Word) extends J {

  val name = "J2P"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.index2)

}

object J2P {

  val operationCode = MixByte(42)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): J2P = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J2P =
    J2P(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}