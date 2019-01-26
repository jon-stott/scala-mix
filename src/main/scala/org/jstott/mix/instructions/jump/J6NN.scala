package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J6NN(override val value: Word) extends J {

  val name = "J6NN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.index6)

}

object J6NN {

  val operationCode = MixByte(46)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): J6NN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J6NN =
    J6NN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}