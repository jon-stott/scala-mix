package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J1Z(override val value: Word) extends J {

  val name = "J1Z"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.index1)

}

object J1Z {

  val operationCode = MixByte(41)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): J1Z = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J1Z =
    J1Z(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}