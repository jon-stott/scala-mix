package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J6P(override val value: Word) extends J {

  val name = "J6P"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.index6)

}

object J6P {

  val operationCode = MixByte(46)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): J6P = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J6P =
    J6P(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}