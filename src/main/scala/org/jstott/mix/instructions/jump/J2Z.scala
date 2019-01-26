package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J2Z(override val value: Word) extends J {

  val name = "J2Z"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.index2)

}

object J2Z {

  val operationCode = MixByte(42)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): J2Z = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J2Z =
    J2Z(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}