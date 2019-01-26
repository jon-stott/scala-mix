package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J5NZ(override val value: Word) extends J {

  val name = "J5NZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.index5)

}

object J5NZ {

  val operationCode = MixByte(45)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): J5NZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J5NZ =
    J5NZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}