package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J5NN(override val value: Word) extends J {

  val name = "J5NN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.index5)

}

object J5NN {

  val operationCode = MixByte(45)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): J5NN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J5NN =
    J5NN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}