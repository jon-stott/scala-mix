package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J3NZ(override val value: Word) extends J {

  val name = "J3NZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.index3)

}

object J3NZ {

  val operationCode = MixByte(43)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): J3NZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J3NZ =
    J3NZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}