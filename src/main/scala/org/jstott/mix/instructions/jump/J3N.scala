package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J3N(override val value: Word) extends J {

  val name = "J3N"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.index3)

}

object J3N {

  val operationCode = MixByte(43)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): J3N = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J3N =
    J3N(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}