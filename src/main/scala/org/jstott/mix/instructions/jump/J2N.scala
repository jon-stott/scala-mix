package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J2N(override val value: Word) extends J {

  val name = "J2N"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.index2)

}

object J2N {

  val operationCode = MixByte(42)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): J2N = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J2N =
    J2N(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}