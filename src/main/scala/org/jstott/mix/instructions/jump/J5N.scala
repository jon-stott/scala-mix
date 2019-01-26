package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J5N(override val value: Word) extends J {

  val name = "J5N"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.index5)

}

object J5N {

  val operationCode = MixByte(45)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): J5N = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J5N =
    J5N(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}