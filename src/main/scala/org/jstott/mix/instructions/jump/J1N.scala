package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J1N(override val value: Word) extends J {

  val name = "J1N"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.index1)

}

object J1N {

  val operationCode = MixByte(41)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): J1N = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J1N =
    J1N(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}