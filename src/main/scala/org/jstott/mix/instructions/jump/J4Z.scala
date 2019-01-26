package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J4Z(override val value: Word) extends J {

  val name = "J4Z"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.index4)

}

object J4Z {

  val operationCode = MixByte(44)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): J4Z = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J4Z =
    J4Z(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}