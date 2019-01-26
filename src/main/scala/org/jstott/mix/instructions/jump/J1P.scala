package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J1P(override val value: Word) extends J {

  val name = "J1P"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.index1)

}

object J1P {

  val operationCode = MixByte(41)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): J1P = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J1P =
    J1P(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}