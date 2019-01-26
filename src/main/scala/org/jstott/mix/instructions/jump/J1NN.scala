package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J1NN(override val value: Word) extends J {

  val name = "J1NN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.index1)

}

object J1NN {

  val operationCode = MixByte(41)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): J1NN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J1NN =
    J1NN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}