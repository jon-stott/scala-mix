package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J3Z(override val value: Word) extends J {

  val name = "J3Z"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.index3)

}

object J3Z {

  val operationCode = MixByte(43)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): J3Z = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J3Z =
    J3Z(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}