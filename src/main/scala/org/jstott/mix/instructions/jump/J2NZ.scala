package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J2NZ(override val value: Word) extends J {

  val name = "J2NZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.index2)

}

object J2NZ {

  val operationCode = MixByte(42)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): J2NZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J2NZ =
    J2NZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}