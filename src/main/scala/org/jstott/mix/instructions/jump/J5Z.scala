package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J5Z(override val value: Word) extends J {

  val name = "J5Z"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.index5)

}

object J5Z {

  val operationCode = MixByte(45)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): J5Z = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J5Z =
    J5Z(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}