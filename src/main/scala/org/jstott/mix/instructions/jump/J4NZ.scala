package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J4NZ(override val value: Word) extends J {

  val name = "J4NZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.index4)

}

object J4NZ {

  val operationCode = MixByte(44)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): J4NZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J4NZ =
    J4NZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}