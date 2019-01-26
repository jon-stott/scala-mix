package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J1NZ(override val value: Word) extends J {

  val name = "J1NZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.index1)

}

object J1NZ {

  val operationCode = MixByte(41)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): J1NZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J1NZ =
    J1NZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}