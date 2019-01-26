package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J4N(override val value: Word) extends J {

  val name = "J4N"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.index4)

}

object J4N {

  val operationCode = MixByte(44)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): J4N = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J4N =
    J4N(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}