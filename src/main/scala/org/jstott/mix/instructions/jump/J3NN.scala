package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J3NN(override val value: Word) extends J {

  val name = "J3NN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.index3)

}

object J3NN {

  val operationCode = MixByte(43)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): J3NN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J3NN =
    J3NN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}