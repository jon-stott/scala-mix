package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J2NN(override val value: Word) extends J {

  val name = "J2NN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.index2)

}

object J2NN {

  val operationCode = MixByte(42)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): J2NN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J2NN =
    J2NN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}