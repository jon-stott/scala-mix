package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J4NN(override val value: Word) extends J {

  val name = "J4NN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.index4)

}

object J4NN {

  val operationCode = MixByte(44)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): J4NN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J4NN =
    J4NN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}